Haskell:
1. always run `hlint .` for linter suggestions, and then use `retrie` to refactor them (Use retrie skill!). If the linter suggestion is too complex, use ast-grep to refactor (Also use skill!).
2. use `floskell` to format code. For example, you can use `find src -type f -name "*.hs" | xargs -I{} -P8 bash -c 'echo Formatting {}; floskell {}'` to batch format.


Baseware:

Always checks UKADOC for baseware specification.

- top of spec: https://ssp.shillest.net/ukadoc/manual/index.html
- structure of base folder: https://ssp.shillest.net/ukadoc/manual/manual_directory.html
- ghost description: https://ssp.shillest.net/ukadoc/manual/descript_ghost.html
- shell description: https://ssp.shillest.net/ukadoc/manual/descript_shell.html
- surface description: https://ssp.shillest.net/ukadoc/manual/descript_shell_surfaces.html
- balloon description: https://ssp.shillest.net/ukadoc/manual/descript_balloon.html
- plugins description: https://ssp.shillest.net/ukadoc/manual/descript_plugin.html
- headline description: https://ssp.shillest.net/ukadoc/manual/descript_headline.html
- install description: https://ssp.shillest.net/ukadoc/manual/descript_install.html
- sakura script commands: https://ssp.shillest.net/ukadoc/manual/list_sakura_script.html
- sakura script property system: https://ssp.shillest.net/ukadoc/manual/list_propertysystem.html
  概要 プロパティシステムはベースウェアが起動中に保持している各種パラメータに、ゴースト側からアクセスして読み書きする仕組みである
- shiori event: https://ssp.shillest.net/ukadoc/manual/list_shiori_event.html
- shiori event external: https://ssp.shillest.net/ukadoc/manual/list_shiori_event_ex.html
  ベースウェア外部のアプリケーションがSSTPによって発信するものや、ゴーストやプラグインが他のゴーストに対して発行するものなど、発行者が外部にあるイベントおよび、それらのイベントの受信者が返却するイベントが含まれます。
- shiori event resource: https://ssp.shillest.net/ukadoc/manual/list_shiori_resource.html
  旧プロトコルSHIORI/2.5において、オーナードローメニューの表示やhomeurlなどの単純な文字列情報を取得するためのリクエストとして定義されたものが元になっています。
  この仕様によって、そうした情報をゴースト側の状態に応じて制御することができます。
- plugin event: https://ssp.shillest.net/ukadoc/manual/list_plugin_event.html
- SHIORI 3.0 protocol: https://ssp.shillest.net/ukadoc/manual/spec_shiori3.html
- SSTP 1.x protocol: https://ssp.shillest.net/ukadoc/manual/spec_sstp.html
- SAORI 1.x protocol: http://www.boreas.dti.ne.jp/~sdn/saori.html
- plugin 2.0 protocol: https://ssp.shillest.net/ukadoc/manual/spec_plugin.html
- headline 2.0 protocol: https://ssp.shillest.net/ukadoc/manual/spec_headline.html
- SHIORI DLL spec: https://ssp.shillest.net/ukadoc/manual/spec_dll.html
- FMO/Mutex: https://ssp.shillest.net/ukadoc/manual/spec_fmo_mutex.html
- x-ukagaka-link スキーム / application/x-nar MIMEタイプ: https://ssp.shillest.net/ukadoc/manual/spec_web.html
