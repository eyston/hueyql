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

var query =
`Users(leebyron, zpao, modocache) {
	id,
	name,
	login,
	created_at {
		year,
		month,
		format(YYYY-MM-dd)
	},
	organizations {
		count,
		edges {
			node {
				id,
				login,
				name
			}
		}
	}
}`;

var QueryExplorerContainer = React.createClass({

	getInitialState: function () {
		return {
			result: null,
			query: query
		};
	},

	handleRun: function (ev) {
		ev.preventDefault();

		var query = this.refs.query.getDOMNode().value.trim();

		this.run(query);
	},

	run: function (query) {
		console.log('query', query);
		request.get('/api/graph').query({query: query, profiled: true}).end(function (res) {

			console.log('response', res);

			if (res.ok) {
				var result = JSON.parse(res.text);
				profile = result['profile-data'];
				delete result['profile-data'];

				this.setState({
					result: result,
					profile: profile
				});
			} else {
				this.setState({
					result: JSON.parse(res.text),
					profile: []
				});
			}


		}.bind(this));
	},

	resultHtml: function () {
		if (this.state.result) {
			return printer.json.prettyPrint(this.state.result)
		}
	},

	componentWillMount: function () {
		console.log('gonna mount!');

		this.run(this.state.query);
	},

	componentDidMount: function() {
		var el = this.getDOMNode();

		this.createChart(el);
	},

	componentDidUpdate: function() {
		var el = this.getDOMNode();
		this.updateChart(el, this.state.profile);
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
		var barHeight = 20;

		var chart = d3.select(el).select('svg').attr('height', barHeight * data.length + 100).select('g');

		data.sort(function (a, b) {
			if (a.start > b.start) {
				return 1;
			}

			if (a.start < b.start) {
				return -1;
			}

			return 0;
		})

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
			.attr('transform', 'translate(200, 0)')
			.attr('height', barHeight / 2)
			.attr('fill', 'steelblue');

		barEnter.append('text')
			.attr('x', 0)
			.attr('y', 7.5);

		bars.attr('transform', function (d, i) { return 'translate(0, ' + i * barHeight + ')'; });

		bars.select('rect')
			.attr('x', function (d) { return x(d.start - start); })
			.attr('width', function (d) { return d3.max([x(d.end - d.start), 1]); })

		bars.select('text')
			.text(function (d) { return d.path; })

	},

	render: function () {

		return (
			<div>
				<header>
					<h1>GithubQL</h1>
					<p>This is just screwing around on mapping a GraphQL'ish type thing to the Github API.</p>
					<p>I have no idea if anything will work other than this default query :)</p>
					<p>Only a few roots work and very few fields are supported on each node, including no `__type__` support.</p>
					<p>Not all requests are non-blocking at the moment which you can notice in the profile graph at the bottom.</p>
				</header>
				<div className="query-explorer-container">
					<div className="query-container">
						<button onClick={this.handleRun}>Run</button>
						<h2>Query</h2>

						<textarea defaultValue={this.state.query} ref="query"></textarea>
					</div>
					<div className="result-container">
						<h2>Result</h2>
						<pre dangerouslySetInnerHTML={{__html: this.resultHtml()}}></pre>
						<h2>Profile</h2>
						<svg></svg>
					</div>
				</div>
			</div>
		);
	}

});

React.render(
	<QueryExplorerContainer />,
	document.getElementById('content')
);
