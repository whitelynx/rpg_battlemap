//----------------------------------------------------------------------------------------------------------------------
// Main Angular application.
//
// @module app.js
//----------------------------------------------------------------------------------------------------------------------

angular.module("battlemap", ['ngResource', 'battlemap.controllers', 'monospaced.mousewheel'])
	.config(['$locationProvider', function($locationProvider) {
		$locationProvider.html5Mode(true);
	}])
	.config(['$routeProvider', function($routeProvider) {
		$routeProvider
			.when('/', {templateUrl: '/partials/list_maps.html',   controller: 'ListMapsCtrl'})
			.when('/maps/:mapid', {templateUrl: '/partials/map.html',   controller: 'ViewMapCtrl'})
			.when('/maps/:mapid/edit', {templateUrl: '/partials/map.html',   controller: 'EditMapCtrl'})
			.otherwise({redirectTo: '/'});
	}])
	.factory('MapSocket', ['$q', '$rootScope', function($q, $rootScope){

		var nextReplyToId = 0;
		var requests = {};
		var mapUrl = false;
		var ws = false;
		var listeners = {};

		var connectingDefer = false;
		var connectingResolved = true;

		var connectMap = function(mapObj){
			if(mapObj.websocketUrl == mapUrl){
				console.log('connection already established', mapObj.websocketUrl);
				return connectingDefer.promise;
			}

			if(ws){
				ws.close();
				if(! connectingResolved){
					$rootScope.$apply(connectingDefer.reject("connection closed"));
				}
			}

			connectingDefer = $q.defer();

			console.log('connecting to map ws', mapObj.websocketUrl, ws ? true : false);

			mapUrl = mapObj.websocketUrl;
			ws = new WebSocket(mapUrl);

			ws.onopen = function(ev){
				console.log('resolving connection', $rootScope);
				connectingResolved = true;
				$rootScope.$apply(connectingDefer.resolve(ev));
				console.log('emiting open socket');
				$rootScope.$apply($rootScope.$emit('mapsocket_open'));
			};

			ws.onmessage = function(ev){
				onMessage(ev);
			};

			ws.onclose = function(ev){
				if(! connectingResolved){
					$rootScope.$apply(connectingDefer.reject("connection closed"));
				}
				mapUrl = false;
				connectinDefer = false;
				connectingResolved = true;
				console.log('connection closing', connectingDefer);
				$rootScope.$apply($rootScope.$emit('mapsocket_closed'));
			}

			return connectingDefer.promise;
		}

		var nextReplyId = function(){
			nextReplyToId++;
			if(nextReplyToId > 10000){
				nextReplyToId = 0;
			}
			return nextReplyToId;
		}

		var sendRequest = function(action, type, id, data){
			var request = {'action':action, 'type': type};
			if(id){
				request.id = id;
			}
			if(data){
				request.data = data;
			}
			request.reply_with = nextReplyId();

			var defer = $q.defer();
			requests[request.reply_with] = {
				'posted': new Date(),
				'defer': defer
			};
			if(connectingDefer){
				connectingDefer.promise.then(function(){
					ws.send(JSON.stringify(request));
				}, function(error){
					defer.reject(error);
				})
			}
			console.log('the defer', defer);
			return defer.promise;
		}

		var onMessage = function(ev){
			var messageObj = JSON.parse(ev.data);
			console.log('messageObj', messageObj);
			if(messageObj.type == 'reply'){
				maybeReply(messageObj);
				return;
			}
			var eventName = messageObj.action + '_' + messageObj.type;
			var emitData = messageObj.action == 'delete' ? messageObj.type_id : messageObj.data;
			console.log('emitting', eventName);
			$rootScope.$emit(eventName, emitData);
		};

		var maybeReply = function(reply){
			console.log('maybe reply', reply);
			if(! reply.type_id){
				return;
			}
			if(requests.hasOwnProperty(reply.type_id)){
				var defer = requests[reply.type_id].defer;
				if(reply.accepted){
					console.log('reply thing', reply.data);
					defer.resolve(reply.data);
				} else {
					defer.reject(reply.data);
				}
				delete requests[reply.type_id];
			}
		};

		var query = function(type){
			var outDefer = $q.defer();
			var q = sendRequest('get', type);
			q.then(function(success){
				success = success.map(function(obj){
					return attach(type, obj);
				});
				outDefer.resolve(success);
			},
			function(fail){
				outDefer.reject(fail);
			});
			return outDefer.promise;
		};

		var get = function(type, id){
			var outDefer = $q.defer();
			var q = sendRequest('get', type, id);
			q.then(function(success){
				attach(type, success);
				/*success.save = function(){
					return sendRequest('put', type, this.id, this);
				};
				success.delete = function(){
					return sendRequest('delete', type, this.id);
				};*/
				outDefer.resolve(success);
			},
			function(fail){
				outDefer.reject(fail);
			});
			return outDefer.promise;
		};

		var attach = function(type, obj){
			obj.$save = function(){
				return sendRequest('put', type, this.id, this);
			};
			obj.$delete = function(){
				return sendRequest('delete', type, this.id);
			};
			var eventName = 'put_' + type
			$rootScope.$on(eventName, function(ev, newObjParams){
				if(newObjParams.id != obj.id){
					return obj;
				}

				var k;
				for(k in newObjParams){
					if(newObjParams.hasOwnProperty(k)){
						obj[k] = newObjParams[k];
					}
				}
				console.log('put event', obj.id, ev, newObjParams);
			});
			return obj;
		};

		connectMap.sendRequest = sendRequest;
		connectMap.query = query;
		connectMap.get = get;
		connectMap.attach = attach;
		return connectMap;
	}])
	.factory('LayerSocket', ['MapSocket', '$q', '$rootScope', function(MapSocket, $q, $rootScope){
		var layers = [];

		var loadLayers = function(){
			MapSocket.query('layer').then(function(success){
				console.log('new layers', success);
				layers.splice(0, layers.length);
				success.forEach(function(e){
					layers.push(e);
				});
			},
			function(fail){
				console.error('could not load layers', fail);
				layers.splice(0, layers.length);
			})
		};

		var create = function(args){
			var retDefer = $q.defer();
			var localDefer = MapSocket.sendRequest('post', 'layer', false, args);
			localDefer.then(function(success){
				success = MapSocket.attach('layer', success);
				layers.push(success);
				retDefer.resolve(success);
			},
			function(fail){
				retDefer.reject(fail);
			})
			return retDefer.promise;
		}

		$rootScope.$on('layer_put', function(ev, obj){
			var foundLayers = layers.filter(function(layer){
				return layer.id == obj.id;
			});
			if(foundLayers.length > 0){
				return;
			}
			obj = MapSocket.attach('layer', obj);
			layers.push(obj);
		});

		$rootScope.$on('layer_delete', function(ev, id){
			console.log('got layer delete', id);
			var found = layers.filter(function(layer){
				return layer.id == id;
			});
			found.forEach(function(l){
				layers.splice(layers.indexOf(l), 1);
			});
		})

		$rootScope.$on('mapsocket_closed', function(){
			layers = layers.splice(0, layers.length);
		});

		$rootScope.$on('mapsocket_open', function(){
			console.log('loading layers in repsonse to open socket');
			loadLayers();
		});

		window.ll = layers;
		return {
			'create':create,
			'layers':layers
		};
	}])
	.run(function($rootScope, $resource){
		$rootScope.user = window.currentUser;
		$rootScope.loginUrl = window.loginUrl;
		$rootScope.logoutUrl = window.logoutUrl;

		$rootScope.Map = $resource('/maps/:mapid', {}, {
		'save': {'method':'PUT'},
		'create':{'method':'POST', },
		'query':{'method':'GET', 'isArray':true, 'params':{'mapid':''}}});

		$rootScope.stopPropagation = function(ev){
			ev.stopPropagation();
		}

	});

//----------------------------------------------------------------------------------------------------------------------
