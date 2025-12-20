# Suggested Commands

## Build and Run
- Build: `stack build`
- Run: `stack exec kokage`
- Repl: `stack repl`

## Testing
- Run tests: `stack test`

## Formatting and Linting
- Lint: `stack exec -- hlint .` (if hlint is installed) or rely on GHC warnings.
- Format: `stack exec -- fourmolu -i .` (if fourmolu is installed) or standard Haskell formatting.

## Project Structure
- `src/`: Library source code
- `app/`: Main executable source
- `test/`: Test suite
- `ghost/`: (Expected) directory for ghost files

## Key Technologies
- Haskell (GHC 9.12.2)
- Stack
- GTK4 (`gi-gtk4`) for UI
- Megaparsec for parsing
