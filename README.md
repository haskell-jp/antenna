# Haskell Antenna

Haskellに関する日本語情報の更新をまとめて表示するWebサイトです。

Collect Haskeller's posts in Japanese (like [Planet Haskell](https://planet.haskell.org/)).

- Posts are collected by [ScrapBook](https://github.com/matsubara0507/scrapbook)
- Forked from [matsubara0507/planet-haskell-jp-demo](https://github.com/matsubara0507/planet-haskell-jp-demo)
- Redesign of [lotz84's Antenna](https://github.com/haskell-jp/antenna/tree/17766399a31bbc7a46423802bead2eccc4dad6a0)

## Usage

Haskell stack を使います

### Build

```
$ git clone https://github.com/haskell-jp/antenna.git
$ cd antenna
$ stack biuld
```

### Run

引数には YAML 形式の設定ファイルを渡します。
設定ファイルの細かいフォーマットは[ココ](https://github.com/matsubara0507/scrapbook/tree/b7cfedba0e34dc117389452b9a61f1e2bbe117fa#documentation)を参照してください．

```
$ stack exec -- antenna sites.yaml
```

実行すると、引数で渡した設定ファイル `sites.yaml` と同じディレクトリに `index.html` と `sites.html` と `feed.xml` が生成されます。

## サイトの追加方法

`sites.yaml` に以下のようにしてフィードの情報を追加して、https://github.com/haskell-jp/antenna/compare より Pull Request を送って下さい。

```yaml
sites:
  ...
  - title: "サイトの名前"
    author: "サイトの作者"
    url: "サイトの URL"
    feed: "フィードの URL"
    logo:
      # 下のいずれか
      url: "ロゴ画像のURL"
      github: "GitHubのアカウント(GitHubのアカウント画像を利用)"
```

現状、フィードは Atom と RSS 2.0 に対応しています。

## アイコンに関するガイドライン

各サービスのアイコンは以下のガイドラインに沿って使用しています。

- Qiita - <http://help.qiita.com/ja/articles/others-brand-guideline>
