var React = require('react');
var Immutable = require('immutable');
var http = require('http');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;


var query = function (component, key) {
	var params = component.queryParams
	return component.queries[key](params);
};


var Store = assign({}, EventEmitter.prototype, {

	data: {},

	fetch: function (graph) {
		var req = http.request({
			method: 'POST',
			path: '/api/graph',
			headers: {'content-type': 'application/json'}
		}, function (res) {
			res.on('data', function (buf) {
				var response = JSON.parse(buf);
				this.data = {};
				response.result.forEach(function (root) {
					this.data[root[0]] = this.data[root[0]] || {};
					if (root[0] === "viewer") {
						this.data[root[0]][root[1]] = root[3];
					} else if (root[0] === "node") {
						this.data[root[0]][root[1]] = root[2];
					}
				}.bind(this));
				this.emitChange();
			}.bind(this));
		}.bind(this));

		req.write(JSON.stringify({
			graph: [query(FriendList, "viewer").toJS()]
		}));

		req.end();

		return {};
	},

	get: function (graph) {

		var type = graph.get(0);
		var id = graph.get(1);

		if (this.data[type] && this.data[type][id]) {
			var result = {};
			result[type] = {}
			result[type][id] = this.data[type][id];
			return result;
		} else {
			return {};
		}
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
				return Immutable.Set.of("user/name", Immutable.List.of("user/mutual-friends", Immutable.Set.of("count")));
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
				return Immutable.Set.of("user/profile-pic");
			}
		}
	},

	render: function () {
		return (
			<img className="profile-pic" src={this.props.user['user/profile-pic']} />
		);
	}
});

var FriendListItem = React.createClass({

	statics: {
		queries: {
			user: function () {
				return Immutable.Set.of("user/is-verified")
					.union(query(ProfilePic, "user"))
					.union(query(FriendInfo, "user"))
			}
		}
	},

	render: function () {
		return (
			<div className="friend-list-item">
				<ProfilePic user={this.props.user} />
				<FriendInfo user={this.props.user} />
			</div>
		);
	}

});


var FriendList = React.createClass({

	statics: {
		queryParams: { count: 20 },
		queries: {
			viewer: function (params) {
				return Immutable.List.of("viewer", "friends", {count: params.count},
					Immutable.Set.of("db/id")
					.union(query(FriendListItem, "user")));
			}
		}
	},

	render: function () {
		return (
			<div className="friend-list">
			{
				this.props.viewer.friends.map(function (user) {
					return <FriendListItem key={user["db/id"]} user={user} />;
				})
			}
			</div>
		);
	}

});

var FriendListContainer = React.createClass({

	getInitialState: function() {
		return  this.stateFromStore();
	},

	componentDidMount: function() {
		Store.addChangeListener(this._onChange);
	},

	componentWillUnmount: function() {
		Store.removeChangeListener(this._onChange);
	},

	stateFromStore: function () {
		return Store.get(query(FriendList, "viewer"));
	},

	_onChange: function () {
		this.setState(this.stateFromStore());
	},

	render: function () {
		if (this.state.viewer) {
			return <FriendList viewer={this.state.viewer} />
		} else {
			return <div></div>;
		}
	}

});

React.render(
	<FriendListContainer />,
	document.getElementById('content')
);

Store.fetch(query(FriendList, "viewer"));
