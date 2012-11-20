var vanguard = {

    getTopologies: function(callback) {
        $.get('/vanguard/topologies', function(data) {
            callback(data.topologies || []);
        });
    },

    setTopology: function(name) {
        document.cookie = 'topology=' + name;
    },

    getTopology: function() {
        key = 'topology';

        regex = new RegExp("(?:^|.*;\\s*)" +
            escape('topology').replace(/[\-\.\+\*]/g, "\\$&") +
            "\\s*\\=\\s*((?:[^;](?!;))*[^;]?).*");

        return unescape(document.cookie.replace(regex, "$1"));
    },

    selectElement: function(id, values) {
        values.push('All');

        cookie = vanguard.getTopology() || 'All';

        options = values.map(function(val, i) {
            return '<option' +
                (val == cookie ? ' selected="selected"' : '') +
                ' value="' + val + '">' + val + '</option>'
        }).reverse().join('');

        return $('<div style="float: left; padding: 4px 16px;"><label for="' + id +
            '">Topology: </label><select id="' + id + '">' +
            options + '</select></div>');
    }

};

$(document).ready(function() {

    vanguard.getTopologies(function(values) {
        id   = 'topologies';
        elem = vanguard.selectElement(id, values);

        $('#login-version').prepend(elem);

        $('#' + id).change(function() {
            vanguard.setTopology($(this).val());

            window.location.href = '';
        });
    });
});
