// var baseURL = "http://localhost:8080"
var baseURL = "https://safe-lowlands-68371.herokuapp.com"

var feed = new Vue({
  el: '#js-feed',
  data: {
    isLoading: true,
    feeds: [],
    sources: {}
  },
  methods: {
    refreshFeed: function() {
      var keys = Object.keys(this.sources);
      if (keys.length == 0) {
        this.feeds = [];
      } else {
        var $this = this;
        this.feeds = keys
          .map(function(key){
            return $this.sources[key];
          })
          .reduce(function(xs, ys){
            return xs.concat(ys);
          })
          .sort(function(_a, _b){
            var a = new Date(_a.datetime);
            var b = new Date(_b.datetime);
            return a.getTime() - b.getTime();
          })
          .reverse();
      }
    },
    addSource: function(label, feed) {
      this.sources[label] = feed.map(function(x){
        x.label = label;
        return x;
      });
      this.refreshFeed();
    },
    removeSource: function(label) {
      delete this.sources[label];
      this.refreshFeed();
    }
  }
})

var menu = new Vue({
  el: '#js-menu',
  data: {
    isLoading: true,
    categories: []
  },
  computed: {
    allLabels: function() {
      return this.getLabelsByCondition(function(_){return true;});
    },
    showingLabels: function() {
      return this.getLabelsByCondition(function(category){
        return category['show-default'];
      });
    }
  },
  methods: {
    setCategories: function(categories) {
      categories.forEach(function(category){
        category.sources.forEach(function(source){
          return source.show = category['show-default'];
        });
      });
      this.categories = categories;
    },
    updateSource: function(label, show) {
      if (show) {
        axios
          .post(baseURL + "/feed/" + label)
          .then(function(res){
            Object.keys(res.data).forEach(function(label){
              this.feed.addSource(label, res.data[label]);
            });
          })
      } else {
        feed.removeSource(label)
      }
    },
    getLabelsByCondition: function(condition) {
      return this.categories
        .map(function(category){
          if (condition(category)) {
            return category.sources
              .map(function(source){
                return source.label;
              });
          } else {
            return [];
          }
        })
        .reduce(function(xs, ys){
          return xs.concat(ys);
        });
    }
  }
})

axios
  .post(baseURL + "/menu")
  .then(function(res){
    menu.setCategories(res.data);
    menu.isLoading = false;
    var labels = menu.showingLabels;
    axios
      .post(baseURL + "/feed/" + labels.join(","))
      .then(function(res){
        Object.keys(res.data).forEach(function(label){
          feed.addSource(label, res.data[label]);
        });
        feed.isLoading = false;
      })
  })

