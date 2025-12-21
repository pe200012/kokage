/*
 * SHIORI Bridge for Wine IPC
 *
 * A daemon that loads SHIORI DLLs and communicates via stdin/stdout.
 * Protocol: Line-based with Base64-encoded payloads for binary safety.
 *
 * Commands:
 *   LOAD <dll_path> <ghost_path>
 *   REQUEST <base64_encoded_request>
 *   UNLOAD
 *   QUIT
 *
 * Compile with: winegcc -o shiori_bridge.exe shiori_bridge.c
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* SHIORI DLL function types */
typedef BOOL (WINAPI *SHIORI_load)(HGLOBAL h, long len);
typedef BOOL (WINAPI *SHIORI_unload)(void);
typedef HGLOBAL (WINAPI *SHIORI_request)(HGLOBAL h, long *len);

/* Global state */
static HMODULE g_hDll = NULL;
static SHIORI_load g_load_fn = NULL;
static SHIORI_unload g_unload_fn = NULL;
static SHIORI_request g_request_fn = NULL;
static int g_loaded = 0;

/* Base64 encoding/decoding tables */
static const char base64_chars[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static int base64_decode_char(char c) {
    if (c >= 'A' && c <= 'Z') return c - 'A';
    if (c >= 'a' && c <= 'z') return c - 'a' + 26;
    if (c >= '0' && c <= '9') return c - '0' + 52;
    if (c == '+') return 62;
    if (c == '/') return 63;
    return -1;
}

/* Base64 encode */
char* base64_encode(const unsigned char* data, size_t len, size_t* out_len) {
    size_t encoded_len = 4 * ((len + 2) / 3);
    char* encoded = (char*)malloc(encoded_len + 1);
    if (!encoded) return NULL;

    size_t i, j;
    for (i = 0, j = 0; i < len;) {
        unsigned int a = i < len ? data[i++] : 0;
        unsigned int b = i < len ? data[i++] : 0;
        unsigned int c = i < len ? data[i++] : 0;
        unsigned int triple = (a << 16) | (b << 8) | c;

        encoded[j++] = base64_chars[(triple >> 18) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 12) & 0x3F];
        encoded[j++] = base64_chars[(triple >> 6) & 0x3F];
        encoded[j++] = base64_chars[triple & 0x3F];
    }

    /* Handle padding */
    size_t mod = len % 3;
    if (mod == 1) {
        encoded[encoded_len - 1] = '=';
        encoded[encoded_len - 2] = '=';
    } else if (mod == 2) {
        encoded[encoded_len - 1] = '=';
    }

    encoded[encoded_len] = '\0';
    *out_len = encoded_len;
    return encoded;
}

/* Base64 decode */
unsigned char* base64_decode(const char* data, size_t len, size_t* out_len) {
    if (len % 4 != 0) return NULL;

    size_t decoded_len = len / 4 * 3;
    if (len > 0 && data[len - 1] == '=') decoded_len--;
    if (len > 1 && data[len - 2] == '=') decoded_len--;

    unsigned char* decoded = (unsigned char*)malloc(decoded_len + 1);
    if (!decoded) return NULL;

    size_t i, j;
    for (i = 0, j = 0; i < len;) {
        int a = data[i] == '=' ? 0 : base64_decode_char(data[i]); i++;
        int b = data[i] == '=' ? 0 : base64_decode_char(data[i]); i++;
        int c = data[i] == '=' ? 0 : base64_decode_char(data[i]); i++;
        int d = data[i] == '=' ? 0 : base64_decode_char(data[i]); i++;

        if (a < 0 || b < 0 || c < 0 || d < 0) {
            free(decoded);
            return NULL;
        }

        unsigned int triple = (a << 18) | (b << 12) | (c << 6) | d;

        if (j < decoded_len) decoded[j++] = (triple >> 16) & 0xFF;
        if (j < decoded_len) decoded[j++] = (triple >> 8) & 0xFF;
        if (j < decoded_len) decoded[j++] = triple & 0xFF;
    }

    decoded[decoded_len] = '\0';
    *out_len = decoded_len;
    return decoded;
}

/* Handle LOAD command */
void cmd_load(const char* dll_path, const char* ghost_path) {
    /* Unload previous if any */
    if (g_loaded && g_unload_fn) {
        g_unload_fn();
    }
    if (g_hDll) {
        FreeLibrary(g_hDll);
        g_hDll = NULL;
    }
    g_loaded = 0;

    /* Load DLL */
    g_hDll = LoadLibraryA(dll_path);
    if (!g_hDll) {
        printf("ERROR LoadLibrary failed: %lu\n", GetLastError());
        fflush(stdout);
        return;
    }

    /* Get function pointers */
    g_load_fn = (SHIORI_load)GetProcAddress(g_hDll, "load");
    g_unload_fn = (SHIORI_unload)GetProcAddress(g_hDll, "unload");
    g_request_fn = (SHIORI_request)GetProcAddress(g_hDll, "request");

    if (!g_load_fn || !g_unload_fn || !g_request_fn) {
        printf("ERROR Missing SHIORI exports\n");
        fflush(stdout);
        FreeLibrary(g_hDll);
        g_hDll = NULL;
        return;
    }

    /* Call load() with ghost path */
    size_t path_len = strlen(ghost_path);
    HGLOBAL path_mem = GlobalAlloc(GMEM_FIXED, path_len + 1);
    if (!path_mem) {
        printf("ERROR GlobalAlloc failed\n");
        fflush(stdout);
        FreeLibrary(g_hDll);
        g_hDll = NULL;
        return;
    }
    strcpy((char*)path_mem, ghost_path);

    BOOL result = g_load_fn(path_mem, (long)path_len);
    GlobalFree(path_mem);

    if (result) {
        g_loaded = 1;
        printf("OK\n");
    } else {
        printf("ERROR load() returned FALSE\n");
        FreeLibrary(g_hDll);
        g_hDll = NULL;
    }
    fflush(stdout);
}

/* Handle REQUEST command */
void cmd_request(const char* base64_request) {
    if (!g_loaded || !g_request_fn) {
        printf("ERROR SHIORI not loaded\n");
        fflush(stdout);
        return;
    }

    /* Decode base64 request */
    size_t req_len;
    unsigned char* req_data = base64_decode(base64_request, strlen(base64_request), &req_len);
    if (!req_data) {
        printf("ERROR Base64 decode failed\n");
        fflush(stdout);
        return;
    }

    /* Log the request */
    FILE* logf = fopen("shiori_log.txt", "a");
    if (logf) {
        fprintf(logf, "=== REQUEST (%zu bytes) ===\n", req_len);
        fwrite(req_data, 1, req_len, logf);
        fprintf(logf, "\n===\n\n");
        fflush(logf);
    }

    /* Allocate and copy request to global memory */
    HGLOBAL req_mem = GlobalAlloc(GMEM_FIXED, req_len + 1);
    if (!req_mem) {
        printf("ERROR GlobalAlloc failed\n");
        fflush(stdout);
        free(req_data);
        if (logf) fclose(logf);
        return;
    }
    memcpy((char*)req_mem, req_data, req_len);
    ((char*)req_mem)[req_len] = '\0';
    free(req_data);

    /* Call request - len is bidirectional */
    long resp_len = (long)req_len;
    HGLOBAL response = g_request_fn(req_mem, &resp_len);
    GlobalFree(req_mem);

    if (response && resp_len > 0) {
        /* Log the response */
        if (logf) {
            fprintf(logf, "=== RESPONSE (%ld bytes) ===\n", resp_len);
            fwrite((char*)response, 1, resp_len, logf);
            fprintf(logf, "\n===\n\n");
            fflush(logf);
        }

        /* Encode response as base64 */
        size_t b64_len;
        char* b64_response = base64_encode((unsigned char*)response, resp_len, &b64_len);
        GlobalFree(response);

        if (b64_response) {
            printf("RESPONSE %s\n", b64_response);
            free(b64_response);
        } else {
            printf("ERROR Base64 encode failed\n");
        }
    } else {
        if (logf) {
            fprintf(logf, "=== RESPONSE (empty) ===\n\n");
        }
        printf("RESPONSE\n");  /* Empty response */
    }

    if (logf) fclose(logf);
    fflush(stdout);
}

/* Handle UNLOAD command */
void cmd_unload(void) {
    if (g_loaded && g_unload_fn) {
        g_unload_fn();
    }
    if (g_hDll) {
        FreeLibrary(g_hDll);
        g_hDll = NULL;
    }
    g_loaded = 0;
    g_load_fn = NULL;
    g_unload_fn = NULL;
    g_request_fn = NULL;
    printf("OK\n");
    fflush(stdout);
}

/* Read a line from stdin */
char* read_line(void) {
    static char buffer[65536];
    if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
        return NULL;
    }
    /* Remove trailing newline */
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
        len--;
    }
    if (len > 0 && buffer[len - 1] == '\r') {
        buffer[len - 1] = '\0';
    }
    return buffer;
}

int main(int argc, char *argv[]) {
    /* Disable output buffering */
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    /* Signal ready */
    printf("READY\n");
    fflush(stdout);

    /* Main command loop */
    while (1) {
        char* line = read_line();
        if (!line) break;

        if (strncmp(line, "LOAD ", 5) == 0) {
            /* Parse: LOAD <dll_path> <ghost_path> */
            char* args = line + 5;
            char* space = strchr(args, ' ');
            if (space) {
                *space = '\0';
                char* dll_path = args;
                char* ghost_path = space + 1;
                cmd_load(dll_path, ghost_path);
            } else {
                printf("ERROR LOAD requires two arguments\n");
                fflush(stdout);
            }
        } else if (strncmp(line, "REQUEST ", 8) == 0) {
            cmd_request(line + 8);
        } else if (strcmp(line, "UNLOAD") == 0) {
            cmd_unload();
        } else if (strcmp(line, "QUIT") == 0) {
            cmd_unload();
            break;
        } else {
            printf("ERROR Unknown command\n");
            fflush(stdout);
        }
    }

    return 0;
}
