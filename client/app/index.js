var React = require('react');
var Immutable = require('immutable');
var http = require('http');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;

require('es6-promise').polyfill();

var query = function (component, key) {
	return component.queries[key]();
};

var request = function (options, body) {
	return new Promise(function(resolve, reject) {
		var chunks = [];

		var req = http.request(options, function (res) {
			res.on('data', function (data) {
				chunks.push(data);
			}.bind(this));

			res.on('end', function () {
				resolve(chunks.join());
			});
		});

		req.on('error', function(e) {
			reject(e);
		});

		if (body) {
			req.write(body);
		}

		req.end();
	});
};


var Store = assign({}, EventEmitter.prototype, {

	data: Immutable.Map(),

	fetch: function (graph) {
		request({
			method: 'POST',
			path: '/api/graph',
			headers: {'content-type': 'application/json'}
		}, JSON.stringify(graph)).then(function (response) {
			var roots = Immutable.fromJS(JSON.parse(response)).get("result");

			roots.forEach(function (root) {
				var key = root.get(0);
				var fields = root.get(1);

				var type = key.get(0);

				switch(type) {
					case "viewer":
						this.data = this.data.set("viewer", fields);
						break;
					case "entity":
						var id = root.get(1);
						this.data = this.data.updateIn(["entity", id], fields);
						break;
					case "node":
						var id = root.get(1);
						this.data = this.data.updateIn(["node", id], fields);
						break;
				}

			}.bind(this));

			this.emitChange();
		}.bind(this));
	},

	get: function () {
		return this.data;
	},

	emitChange: function() {
		this.emit("change");
	},

	addChangeListener: function(callback) {
		this.on("change", callback);
	},

	removeChangeListener: function(callback) {
		this.removeListener("change", callback);
	}

});

var FriendInfo = React.createClass({

	statics: {
		queries: {
			user: function () {
				return [
					"user/name",
					["user/mutual-friends", ["count"]]
				];
			}
		}
	},

	render: function () {
		return (
			<div className="friend-info">
				<div className="name">{this.props.user["user/name"]}</div>
				<div className="mutual-friends">{this.props.user["user/mutual-friends"].count} mutual friends</div>
			</div>
		);
	}
});

var ProfilePic = React.createClass({

	statics: {
		queries: {
			user: function () {
				return ["user/profile-pic"];
			}
		}
	},

	render: function () {
		return (
			<img className="profile-pic" src={this.props.user["user/profile-pic"]} />
		);
	}
});

var FriendListItem = React.createClass({

	statics: {
		queries: {
			user: function () {
				return ["user/is-verified"]
					.concat(query(ProfilePic, "user"))
					.concat(query(FriendInfo, "user"));
			}
		}
	},

	render: function () {
		var verifiedBadge;

		if (this.props.user["user/is-verified"]) {
			verifiedBadge = <div className="verified">V</div>;
		}

		return (
			<div className="friend-list-item">
				{verifiedBadge}
				<ProfilePic user={this.props.user} />
				<FriendInfo user={this.props.user} />
			</div>
		);
	}

});


var FriendList = React.createClass({

	statics: {
		queries: {
			viewer: function () {
				return [["user/friends",
					[["edges",
						[["node",
							["db/id"].concat(query(FriendListItem, "user"))]
						]
					]]
				]];
			}
		}
	},

	render: function () {
		return (
			<div className="friend-list">
			{
				this.props.viewer["user/friends"].edges.map((edge) => {
					return <FriendListItem key={edge.node["db/id"]} user={edge.node} />;
				})
			}
			</div>
		);
	}

});

var FriendListContainer = React.createClass({

	getInitialState: function() {
		return Store.get();
	},

	shouldComponentUpdate: function (props, state) {
		return !this.state.equals(state);
	},

	componentDidMount: function() {
		Store.addChangeListener(this._onChange);
	},

	componentWillUnmount: function() {
		Store.removeChangeListener(this._onChange);
	},

	_onChange: function () {
		this.replaceState(Store.get());
	},

	render: function () {
		if (this.state.has("viewer")) {
			return <FriendList viewer={this.state.toJS().viewer} />
		} else {
			return <div></div>;
		}
	}

});

React.render(
	<FriendListContainer />,
	document.getElementById('content')
);

Store.fetch([[["viewer"], query(FriendList, "viewer")]]);
