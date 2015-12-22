// module Control.UI.Browser

exports.replaceLocation = function(url) {
    return function() {
        window.location.replace(url);
    };
};

exports.setLocation = function(url) {
    return function() {
        window.location.assign(url);
    };
};

exports.locationString = function() {
    var path = window.location.pathname.split("/");
    path.pop();
    return window.location.origin + path.join("/");
};

exports.select = function(el) {
    return function() {
        el.select();
    };
};
exports.newTab = function(url) {
    return function() {
        window.open(url, "_blank");
    };
};

exports.reload = function() {
    document.location.reload();
};

exports.clearValue = function(el) {
    return function() {
        el.value = null;
    };
};

exports.setTitle = function(t) {
    return function() {
        document.title = t;
    };
};

exports.encodeURIComponent = function(str) {
    if (typeof window !== "undefined") {
        return window.encodeURIComponent(str);
    } else {
        return global.encodeURIComponent(str);
    }
};

exports.decodeURIComponent = function(str) {
    if (typeof window !== "undefined") {
        return window.decodeURIComponent(str);
    } else {
        return global.decodeURIComponent(str);
    }
};