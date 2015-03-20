var React = require('react');
var request = require('superagent');

// http://jsfiddle.net/unLSJ/
var printer = {};
printer.json = {
    replacer: function(match, pIndent, pKey, pVal, pEnd) {
        var key = '<span class=json-key>';
        var val = '<span class=json-value>';
        var str = '<span class=json-string>';
        var r = pIndent || '';
        if (pKey)
            r = r + key + pKey.replace(/[": ]/g, '') + '</span>: ';
        if (pVal)
            r = r + (pVal[0] == '"' ? str : val) + pVal + '</span>';
        return r + (pEnd || '');
    },
    prettyPrint: function(obj) {
        var jsonLine = /^( *)("[\w]+": )?("[^"]*"|[\w.+-]*)?([,[{])?$/mg;
        return JSON.stringify(obj, null, 3)
            .replace(/&/g, '&amp;').replace(/\\"/g, '&quot;')
            .replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(jsonLine, printer.json.replacer);
    }
};

var QueryEditor = React.createClass({

    handleRun: function (ev) {
        ev.preventDefault();

        var query = this.refs.query.getDOMNode().value.trim();

        this.props.run(query);
    },

    render: function () {
        return (
            <form className="query-editor" onSubmit={this.handleRun}>
                <button>RUN</button>
                <h4>QUERY</h4>

                <textarea defaultValue={this.props.query} ref="query"></textarea>
            </form>
        );
    }

});

var ResultView = React.createClass({

    resultHtml: function () {
        if (this.props.result) {
            return printer.json.prettyPrint(this.props.result)
        }
    },

    render: function () {
        return (<pre dangerouslySetInnerHTML={{__html: this.resultHtml()}}></pre>);
    }
});

var ProfileView = React.createClass({

    componentDidMount: function() {
        var el = this.getDOMNode();

        this.createChart(el);
        this.updateChart(el, this.props.profile);
    },

    componentDidUpdate: function() {
        var el = this.getDOMNode();
        this.updateChart(el, this.props.profile);
    },

    createChart: function (el) {
        var chart = d3.select(el).select('svg').attr('width', 800)
            .append('g').attr('transform', 'translate(10, 10)');

        chart.append('g')
            .attr('class', 'x axis')
            .attr('transform', 'translate(200, 20)')

        chart.append('g')
            .attr('class', 'bars')
            .attr('transform', 'translate(0, 30)')
    },

    updateChart: function (el, data) {
        console.log('data', data);
        var barHeight = 20;

        var chart = d3.select(el).select('svg').attr('height', barHeight * data.length + 50).select('g');

        data.sort(function (a, b) {
            if (a.start > b.start) {
                return 1;
            }

            if (a.start < b.start) {
                return -1;
            }

            return 0;
        });

        function appendChildren(acc, path, data) {
            var children = data.filter(function (d) {
                return d.parent === path;
            });

            children.forEach(function (d) {
                acc.push(d);
                appendChildren(acc, d.path, data);
            });

            return acc;
        }

        data = appendChildren([], "[]", data);

        var start = d3.min(data, function (d) { return d.start; });
        var end = d3.max(data, function (d) { return d.end; });

        var x = d3.scale.linear()
            .domain([0, end - start])
            .range([0, 500]);

        var axis = d3.svg.axis()
            .scale(x)
            .orient('top');

        chart.select('.x.axis').call(axis);

        var bars = chart.select('.bars').selectAll('.bar').data(data);

        bars.exit().remove();

        var barEnter = bars.enter().append('g').attr('class', 'bar');

        barEnter.append('rect')
            .attr('class', 'end')
            .attr('transform', 'translate(200, 0)')
            .attr('height', barHeight / 2);

        barEnter.append('rect')
            .attr('class', 'execute')
            .attr('transform', 'translate(200, 0)')
            .attr('height', barHeight / 2);

        barEnter.append('text')
            .attr('x', 0)
            .attr('y', 7.5);

        bars.attr('transform', function (d, i) { return 'translate(0, ' + i * barHeight + ')'; });

        bars.select('rect.end')
            .attr('x', function (d) { return x(d.start - start); })
            .attr('width', function (d) { return d3.max([x(d.end - d.start), 1]); })

        bars.select('rect.execute')
            .attr('x', function (d) { return x(d.start - start); })
            .attr('width', function (d) { return d3.max([x(d.execute - d.start), 1]); })

        bars.select('text')
            .text(function (d) { return d.path; })

    },

    render: function () {
        return (
            <div className="profile-view">
                <svg></svg>
            </div>
        );
    }

});

var ResultContainer = React.createClass({

    onSetResult: function (ev) {
        ev.preventDefault();
        this.props.setResultTab("result");
    },

    onSetProfile: function (ev) {
        ev.preventDefault();
        this.props.setResultTab("profile");
    },

    render: function () {
        var view;

        if (this.props.error) {
            view = (<div className="error-view">{this.props.error}</div>);
        } else {
            view = this.props.resultTab === "profile"
                ? (<ProfileView profile={this.props.profile} />)
                : (<ResultView result={this.props.result} />);
        }

        return (
            <div className="result-container">
                <button onClick={this.onSetResult}>RESULT</button>
                <button onClick={this.onSetProfile}>PROFILE</button>
                {view}
            </div>
        );
    }

});

var QueryExampleContainer = React.createClass({

    getInitialState: function () {
        return {
            query: queries[this.props.initialQuery] || "",
            resultTab: this.props.initialResultTab || "result"
        };
    },

    componentWillMount: function () {
        if (this.props.autorun) {
            this.run(this.state.query);
        }
    },

    run: function (query) {
        request.get('/api/graph').query({query: query}).end(function (res) {

            if (res.ok) {
                var result = JSON.parse(res.text);
                profile = result['profile-data'];
                delete result['profile-data'];

                this.setState({
                    query: query,
                    result: result,
                    profile: profile,
                    error: null
                });
            } else {
                this.setState({
                    query: query,
                    result: null,
                    error: res.text ? JSON.parse(res.text) : "Unknown error :(",
                    profile: []
                });
            }


        }.bind(this));
    },

    setResultTab: function (tab) {
        this.setState({
            resultTab: tab
        });
    },

    render: function () {
        return (
            <div className="query-container">
                <div className="col33">
                    <QueryEditor query={this.state.query} run={this.run} />
                </div>
                <div className="col67">
                    <ResultContainer
                        result={this.state.result}
                        profile={this.state.profile}
                        error={this.state.error}
                        resultTab={this.state.resultTab}
                        setResultTab={this.setResultTab} />
                </div>
            </div>
        )
    }
});

var queries = {
    simple: `
Repository(facebook/react) {
    id,
    name,
    full_name,
    private,
    html_url,
    description,
    fork,
    url,
    homepage,
    open_issues,
    default_branch
}`,
    nested: `
Repository(facebook/react) {
    id,
    name,
    full_name,
    description,
    owner {
        id,
        login,
        type
    }
}`,
    nestedmult: `
Repository(facebook/react) {
    id,
    name,
    full_name,
    description,
    owner {
        id,
        login,
        type
    },
    organization {
        id,
        name,
        login,
        location,
        type,
        url
    }
}`,
    nestedDate: `
Repository(facebook/react) {
    id,
    name,
    full_name,
    description,
    created_at {
        month,
        day,
        year
    },
    updated_at {
        month,
        day,
        year
    }
}`,
    collection: `
Repository(facebook/react) {
    id,
    name,
    full_name,
    description,
    contributors.first(10) {
        count,
        edges {
            cursor,
            node {
                id,
                name,
                login
            }
        }
    }
}`,
    pagination: `
Repository(facebook/react) {
    id,
    name,
    full_name,
    description,
    contributors.after(197597).first(10) {
        count,
        edges {
            cursor,
            node {
                id,
                name,
                login
            }
        }
    }
}`,
    startswith: `
Organization(facebook) {
    id,
    name,
    login,
    description,
    repositories.starts_with(r).first(5) {
        edges {
            node {
                id,
                name,
                full_name,
                description
            }
        }
    }
}`,
    dateformat: `
User(zpao) {
    id,
    name,
    login,
    location,
    created_at {
        month,
        day,
        year,
        format(YYYY-MM-dd)
    }
}`,
    multipleusers: `
Users(zpao, modocache, leebyron) {
    id,
    name,
    login,
    location,
    repositories.first(5) {
        count,
        edges {
            node {
                id,
                full_name
            }
        }
    },
    organizations.first(5) {
        count,
        edges {
            node {
                id,
                name
            }
        }
    }
}`,
    type: `
Repository(facebook/react) {
    __type__ {
        name,
        description,
        fields {
            name,
            type,
            description
        }
    }
}`,
    validation: `
User(zpao) {
    id,
    name,
    repositories {
        count,
        edges {
            cursor,
            node {
                id,
                name,
                fake_field
            }
        }
    }
}`,
    closing: `
User(zpao) {
    login,
    id,
    avatar_url,
    gravatar_id,
    url,
    html_url,
    followers.first(5) { count, edges { cursor, node { id, name, login } } },
    following.first(5) { count, edges { cursor, node { id, name, login } } },
    starred.first(5) { count, edges { cursor, node { id, name, full_name } } },
    subscriptions.first(5) { count, edges { cursor, node { id, name, full_name } } },
    repositories.first(5) { count, edges { cursor, node { id, name, full_name } } },
    type,
    site_admin,
    name,
    company,
    blog,
    location,
    email,
    hireable,
    bio,
    created_at { month, day, year },
    updated_at { month, day, year }
}`,
    facebook: `
Facebook {
    id,
    name,
    login,
    description,
    location,
    public_members.first(10) {
        count,
        edges {
            cursor,
            node {
                id,
                name,
                location,
                repositories {
                    count
                }
            }
        }
    },
    repositories.first(10) {
        count,
        edges {
            cursor,
            node {
                id,
                name,
                full_name
            }
        }
    }
}`

};


React.render(
    <QueryExampleContainer initialQuery={"simple"} initialResultTab={"result"} autorun={true} />,
    document.getElementById('simple-query')
);

React.render(
    <QueryExampleContainer initialQuery={"nested"} />,
    document.getElementById('nested-query')
);

React.render(
    <QueryExampleContainer initialQuery={"nestedmult"} />,
    document.getElementById('nested-mult-query')
);

React.render(
    <QueryExampleContainer initialQuery={"nestedDate"} />,
    document.getElementById('date-query')
);

React.render(
    <QueryExampleContainer initialQuery={"collection"} />,
    document.getElementById('collection-query')
);

React.render(
    <QueryExampleContainer initialQuery={"pagination"} />,
    document.getElementById('pagination-query')
);

React.render(
    <QueryExampleContainer initialQuery={"startswith"} />,
    document.getElementById('starts-with-query')
);

React.render(
    <QueryExampleContainer initialQuery={"dateformat"} />,
    document.getElementById('date-format-query')
);

React.render(
    <QueryExampleContainer initialQuery={"multipleusers"} />,
    document.getElementById('multiple-users-query')
);

React.render(
    <QueryExampleContainer initialQuery={"type"} />,
    document.getElementById('type-query')
);

React.render(
    <QueryExampleContainer initialQuery={"validation"} />,
    document.getElementById('validation-query')
);

React.render(
    <QueryExampleContainer initialQuery={"closing"} />,
    document.getElementById('closing-query')
);

React.render(
    <QueryExampleContainer initialQuery={"facebook"} />,
    document.getElementById('facebook-query')
);

