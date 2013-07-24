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
			return defer.promise;
		}

		var onMessage = function(ev){
			$rootScope.$apply(function(){
				var messageObj = JSON.parse(ev.data);
				if(messageObj.type == 'reply'){
					maybeReply(messageObj);
					return;
				}
				var eventName = messageObj.action + '_' + messageObj.type;
				var emitData = messageObj.action == 'delete' ? messageObj.type_id : messageObj.data;
				$rootScope.$emit(eventName, emitData);
			});
		};

		var maybeReply = function(reply){
			if(! reply.type_id){
				return;
			}
			if(requests.hasOwnProperty(reply.type_id)){
				var defer = requests[reply.type_id].defer;
				if(reply.accepted){
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
	.factory('SocketResource', ['MapSocket', '$q', '$rootScope', function(MapSocket, $q, $rootScope){

		return function(typeString){
			var localArray = [];

			var safePush = function(newObj){
				var foundObjs = localArray.filter(function(obj){
					return obj.id == newObj.id;
				});
				if(foundObjs.length > 0){
					return foundObjs[0];
				}
				newObj = MapSocket.attach(typeString, newObj);
				localArray.push(newObj);
				return newObj;
			}

			var loadObjects = function(){
				MapSocket.query(typeString).then(function(success){
					localArray.splice(0, localArray.length);
					success.forEach(function(e){
						localArray.push(e);
					});
				},
				function(fail){
					console.error('socket res failed to load data', typeString, fail);
					localArray.splice(0, localArray.length)
				})
			};

			var create = function(args){
				var retDefer = $q.defer();
				var localDefer = MapSocket.sendRequest('post', typeString, false, args);
				localDefer.then(function(success){
					success = safePush(success);
					retDefer.resolve(success);
				},
				function(fail){
					retDefer.reject(fail);
				});
				return retDefer.promise;
			};

			$rootScope.$on('put_' + typeString, function(ev, obj){
				safePush(obj);
			});

			$rootScope.$on('delete_' + typeString, function(ev, id){
				var found = localArray.filter(function(obj){
					return obj.id == id;
				});
				found.forEach(function(o){
					localArray.splice(localArray.indexOf(o), 1);
				});
			});

			$rootScope.$on('mapsocket_closed', function(){
				localArray.splice(0, localArray.length);
			});

			$rootScope.$on('mapsocket_open', function(){
				loadObjects();
			});

			return {
				'create': create,
				'models': localArray
			};
		}
	}]).
	factory('Tool', ['$rootScope', function($rootScope){
		var defaultTool = {
			name: 'default',
			selected: function(){
				return true;
			},
			deselected: function(){
				return true;
			},
			grid_mousedown: function(origin, ev, cellx, celly){
				return true;
			},
			grid_mouseup: function(origin, ev, cellx, celly){
				return true;
			},
			grid_mousemove: function(origin, ev, cellx, celly){
				return true;
			}
		};

		var currentTool = defaultTool;

		var setTool = function(makeFunc){
			var newTool = Object.create(defaultTool);
			currentTool.deselected();
			updateTool = makeFunc(newTool);
			updateTool.selected();
			$rootScope.$broadcast('grid_toolchange', updateTool, currentTool);
			currentTool = updateTool;
			return currentTool;
		};

		setTool.currentTool = currentTool;

		$rootScope.$on('grid_mousedown', function(){
			currentTool.grid_mousedown.apply(currentTool, arguments);
		});

		$rootScope.$on('grid_mousemove', function(){
			currentTool.grid_mousemove.apply(currentTool, arguments);
		});

		$rootScope.$on('grid_mouseup', function(){
			currentTool.grid_mouseup.apply(currentTool, arguments);
		});

		return setTool;
	}])
	.run(function($rootScope, $resource, SocketResource){
		$rootScope.user = window.currentUser;
		$rootScope.loginUrl = window.loginUrl;
		$rootScope.logoutUrl = window.logoutUrl;

		$rootScope.Map = $resource('/maps/:mapid', {}, {
		'save': {'method':'PUT'},
		'create':{'method':'POST', },
		'query':{'method':'GET', 'isArray':true, 'params':{'mapid':''}}});

		$rootScope.Layers = SocketResource('layer');
		$rootScope.Combatants = SocketResource('combatant');
		$rootScope.Zones = SocketResource('zone');
		$rootScope.Auras = SocketResource('aura');
		$rootScope.Scenery = SocketResource('scenery');

		$rootScope.stopPropagation = function(ev){
			ev.stopPropagation();
		};

		$rootScope.nearest = function(i, fractions){
			if(! fractions){
				return Math.round(i);
			}
			return Math.round(i * fractions) / fractions;
		};

		$rootScope.contained = function(i, fractions){
			if(! fractions){
				return Math.floor(i);
			}
			return Math.floor(i * fractions) / fractions;
		};

		$rootScope.translate = {x:0,y:0};
		$rootScope.scale = 1;
		$rootScope.CELL_SIZE = 32;

		$rootScope.transform = function(n, trans){
			return (n + trans) * $rootScope.scale;
		};

		$rootScope.transformX = function(n){
			return $rootScope.transform(n, $rootScope.translate.x);
		};

		$rootScope.transformY = function(n){
			return $rootScope.transform(n, $rootScope.translate.y);
		};

		$rootScope.dimensionToCell = function(d, pan){
			return ( (d - pan) / $rootScope.scale) / $rootScope.CELL_SIZE;
		};

		$rootScope.pixelsToCells = function(pixelX, pixelY){
			var cellX = $rootScope.dimensionToCell(pixelX, $rootScope.translate.x);
			var cellY = $rootScope.dimensionToCell(pixelY, $rootScope.translate.y);
			return [cellX, cellY];
		};

	});

//----------------------------------------------------------------------------------------------------------------------
