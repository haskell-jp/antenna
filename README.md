Haskell Antenna
===============

Haskellに関する日本語情報の更新をまとめて表示するWebサイト／APIです。

使い方
------

事前にlocalhost:6379でRedis Serverをたてておく必要があります。

```shell
$ stack build
$ stack exec antenna-exe -- --env=local
```

Haddock
-------

```shell
$ stack haddock --open antenna
```

フィードの追加方法
------------------

1. config/feed.yaml に情報を記載
2. static/image/logo にアイコンを追加
3. <https://github.com/lotz84/antenna/compare> よりPull Requestを送って下さい

アイコンに関するガイドライン
----------------------------

各サービスのアイコンは以下のガイドラインに沿って使用しています。

* Qiita - <http://help.qiita.com/ja/articles/others-brand-guideline>
* Reddit - <https://www.reddit.com/wiki/licensing>
* Stack Exchange - <https://stackexchange.com/legal/trademark-guidance>

----

Inspired by

- [Awesome Haskell](https://haskell.libhunt.com/)
- [Haskell - News](https://www.haskell.org/news)

