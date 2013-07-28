//----------------------------------------------------------------------------------------------------------------------
// Main page controllers for Battlemap
//
// @module battlemap.controllers.js
//----------------------------------------------------------------------------------------------------------------------

Controllers = angular.module("battlemap.controllers", []);

//----------------------------------------------------------------------------------------------------------------------

Controllers.controller("PersonaCtrl", function($scope, $rootScope){

	// why yes, pulling this from the window is a horrible idea.
	$scope.signin = function(){
		navigator.id.request();
	}

	$scope.signout = function(){
		navigator.id.logout();
	}

	$scope.user = window.currentUser;
	$scope.loginUrl = window.loginUrl;
	$scope.logoutUrl = window.logoutUrl;

	navigator.id.watch({
		loggedInUser: $scope.user,
		onlogin: function(assertion){
			console.log('onlogin', $scope.user);
			$.ajax({
				type: 'POST',
				url: $scope.loginUrl,
				data: JSON.stringify({assertion: assertion}),
				contentType: 'application/json',
				success: function(res, status, xhr){ window.location.reload(); },
				error: function(xhr, status, err){ console.log('login fail', err); }
			});
		},
		onlogout: function(){
			console.log('onlogout', $scope.currentUser);
			$.ajax({
				type: 'POST',
				url: $scope.logoutUrl,
				contentType: 'application/json',
				success: function(res, status, xhr){ window.location.reload(); },
				error: function(xhr, status, err){ console.log('logout fail', err); }
			});
		}

	});

});

Controllers.controller("ListLayersCtrl", function($scope, $rootScope){
	//console.log('layers list ctrl', $scope, $scope.map.id);

	$scope.layers = $rootScope.Layers.models;

	$scope.$watch('layers.length', function(newVal, oldVal){
		console.log('layers length watch', newVal, oldVal);
		if(newVal > 0 && oldVal == 0){
			$scope.layers[0].visible = true;
			$scope.selected = $scope.layers[0];
		}
	});

	$scope.newLayer = function(layerName){
		var defer = $rootScope.Layers.create({'name':layerName});
		defer.then(function(success){
			success.visible = true;
			$scope.selected = success;
			$scope.new_layer_name = '';
		},
		function(fail){
			console.error('could not make new layer', fail);
			$scope.new_layer_name = '';
		});
	};

	$scope.removeLayer = function(layer){
		var defer = layer.$delete();
		defer.then(function(success){
			return true;
		},
		function(fail){
			console.error('could not delete layer', layer, fail);
		});
	}

	$scope.selectLayer = function(layer){
		$scope.layers.selected = layer;
	}

	$scope.isVisible = function(layer){
		return layer.visible;
	}
});

Controllers.controller("ListCombatantsCtrl", function($scope, $rootScope){
	$scope.combatants = $rootScope.Combatants.models;
	$scope.layers = $rootScope.Layers.models;

	$scope.removeCombatant = function(combatant){
		var defer = combatant.$delete();
		defer.then(function(success){
			return true;
		},
		function(fail){
			console.error('could not delete combatant', combatant, fail);
		});
	};

	$scope.selectCombatant = function(combatant, ev){
		$scope.combatants.selected = combatant;
		if(ev){
			var dragData = {};
			var moveCombatantOverride = {
				'grid_mousedown':function(_origin, _ev, cellX, cellY){
					if(isNaN(combatant.x)){
						combatant.x = 0;
					}
					if(isNaN(combatant.y)){
						combatant.y = 0;
					}
					dragData.lastX = combatant.x - Math.floor(cellX);
					dragData.lastY = combatant.y - Math.floor(cellY);
					console.log('mouse down', dragData, combatant);
				},
				'grid_mousemove': function(_origin, _ev, cellX, cellY){
					combatant.x = Math.floor(cellX) + dragData.lastX;
					combatant.y = Math.floor(cellY) + dragData.lastY;
					combatant.$save();
					console.log('mousemove', dragData);
				}
			};
			if(ev.overrideTool){
				console.log('override tool already in place');
				return;
			}
			ev.overrideTool = moveCombatantOverride;
		}
	};

	$scope.saveCombatant = function(ev, combatant){
		var defer = combatant.$save();
		defer.then(function(success){
			return true;
		},
		function(fail){
			console.error('could not save combatant', fail, combatant);
		});
	};

	$scope.deleteCombatant = function(ev, combatant){
		if(ev){
			$rootScope.stopPropagation(ev);
		}

		var defer = combatant.$delete();
		defer.then(function(success){
			if($scope.combatants.selected == combatant){
				$scope.combatants.selected = null;
			}
		},
		function(fail){
			console.error('could not delete combatant', fail, combatant);
		})
	};

});

Controllers.controller("ListZonesCtrl", function($scope, $rootScope){
	$scope.zones = $rootScope.Zones.models;

	$scope.$on('selectzone', function(ev, zone) {
		$scope.selectZone(zone);
	});

	$scope.removeZone = function(zone, ev){
		if(ev){
			$rootScope.stopPropagation(ev);
		}

		var defer = zone.$delete();
		defer.then(function(success){
			if($scope.zones.selected == zone){
				$scope.zones.selected = null;
			}
			return true;
		},
		function(fail){
			console.error('could not delete zone', fail);
		});
	};

	$scope.selectZone = function(zone, ev){
		$scope.zones.selected = zone;
		if(ev){
			ev.grid_captor = zone;
		}
	};

	$scope.saveZone = function(zone, ev){
		var defer = zone.$save();
		defer.then(function(success){
			return true;
		},
		function(fail){
			console.error('could not save zone', fail);
		});
	};

	$scope.pointString = function(zone){
		return zone.points.map(function(p){
			var x = p.x * 32;
			var y = p.y * 32;
			return x +',' + y;
		}).join(' ');
	};

	$scope.setToolOverride = function(zone, ev){
		if(zone != $scope.zones.selected){
			return;
		}
		if(ev.overrideTool){
			return;
		}
		var dragData = {};
		var override = {
			'grid_mousedown': function(origin, ev, cellX, cellY){
				var xp = 'x';
				var yp = 'x'
				switch(zone.shape){
					case 'circle':
						xp = 'cx';
						yp = 'cy';
						break;
					default:
						xp = 'x';
						yp = 'y';
				}
				dragData.lastX = $rootScope.nearest(zone[xp] - cellX, 2);
				dragData.lastY = $rootScope.nearest(zone[yp] - cellY, 2);
			},
			'grid_mousemove': function(origin, ev, cellX, cellY){
				var xp, yp;
				switch(zone.shape){
					case 'rect':
						xp = 'x';
						yp = 'y';
						break;
					case 'circle':
						xp = 'cx';
						yp = 'cy';
						break;
				}
				zone[xp] = $rootScope.nearest(cellX + dragData.lastX, 2);
				zone[yp] = $rootScope.nearest(cellY + dragData.lastY, 2);
				var defer = zone.$save();
				defer.then(function(success){
					$rootScope.$broadcast('zone_moved', zone);
				},
				function(fail){
					console.log('zone move failed', fail);
				})
			}
		};
		ev.overrideTool = override;
	};

});

Controllers.controller("ListMapsCtrl", function($scope, $rootScope, $resource) {
	// the resource thing doesn't really do hateaos well, but then again
	// neither does the browser. ah well.
	$rootScope.maps = [];

	var mapsPromise = $rootScope.Map.query();
	mapsPromise.$then(function(success){
		$rootScope.maps = success.data;
	}, function(error){
		console.error('failed to get maps', error);
	});

	// Disable the toolbar from the main nav bar.
	$scope.noToolbar = true;

	// Get the maps we're participating in.
	$scope.getParticipating = function(maps) {
		return maps;
	};

	$scope.createMap = function(){
		var mapPromise = $rootScope.Map.create({name: $scope.newMapName});
		mapPromise.$then(function(success){
			$rootScope.maps.push(success.data);
			$scope.newMapName = '';
		},
		function(error){
			console.error('could not make map', error);
			$scope.newMapName = '';
		})
	};

	$scope.deleteMap = function(map){
		var delPromise = $rootScope.Map.delete({mapid: map.id});
		delPromise.$then(function(success){
			var index = $rootScope.maps.indexOf(map);
			if(index < 0) {
				return;
			}
			$rootScope.maps.splice(index, 1);
		})
	};

});

Controllers.controller("ViewMapCtrl", function($scope, $routeParams, $rootScope, $resource, MapSocket, Tool) {
	$scope.map = {};

	$scope.noToolbar = false;

	var whenInvitesReady = function(partier){
		var def = MapSocket.sendRequest('post', 'map', $scope.map.id, {'action':'invite','args':[partier]});
		def.then(function(success){
			console.log('invited partier', partier);
		},
		function(failed){
			console.error('failure to invite', failed);
		});
	};

	var mapPromise = $rootScope.Map.get($routeParams);
	mapPromise.$then(function (success) {
		console.log('map data gotten');
		$scope.map = success.data;

		var connectDefer = MapSocket($scope.map);
		connectDefer.then(function(success){
			console.log('connect defer');
			var socketPromise = MapSocket.get('map', parseInt($routeParams.mapid, 10));
			console.log('der promise', socketPromise);
			socketPromise.then(function(success){
				console.log('der success', success);
				$scope.map = success;
				$scope.invite = whenInvitesReady;
			},
			function(fail){
				console.log('failed to get map', fail);
			})
		},
		function(fail){
			console.error("websocket failed to connect", fail);
		})
	},
	function(error){
		console.error('some error', error);
	});

	$scope.saveMap = function(ev){
		console.log('saving map', $scope.map);
		$scope.map.$save();
	};

	$scope.mapBackgroundCssObject = function(){
		var out = {'backgroundColor': $scope.map.background_color};
		return out;
	};

	$scope.invite = function(partier){
		console.error('not yet ready for invites');
	};
});

Controllers.controller("AddCombatantToolCtrl", function($scope, $rootScope, Tool){
	$scope.$on('grid_toolchange', function(ev, newTool){
		$scope.toolName = newTool.name;
	});

	$scope.name_base = 'Fight Man';
	$scope.size = 1;
	$scope.color = '#00ff00';
	$scope.aura_size = 0;
	$scope.aura_color = '#009900';

	var makeAddCombatantTool = function(def){
		def.name = 'Add Combatant';
		var madeCombatant = false;
		var mouseIsDown = false;

		def.grid_mousedown = function(origin, ev, cellX, cellY){
			if($scope.name_base == ""){
				return;
			}

			mouseIsDown = true;
			var x = Math.floor(cellX);
			var y = Math.floor(cellY);

			var layerId = null;
			if($rootScope.Layers.models.selected){
				layerId = $rootScope.Layers.models.selected.id;
			} else {
				layerId = $rootScope.Layers.models.reduceRight(function(red, layer){
					if(layer.visible){
						return red || layer.id;
					}
				}, layerId)
			}

			var regEx = RegExp("^" + $scope.name_base + "( \\d+)?$");
			var reduceFunc = function(red, combatant){
				var gotNum = 1;
				var match = regEx.exec(combatant.name);
				if(match){
					if(match[1] == null){
						gotNum = 1;
					} else {
						gotNum = parseInt(match[1], 10);
					}
					if(gotNum < red){
						return red;
					}
					return gotNum + 1;
				}
				return red;
			}
			var maybeAppend = $rootScope.Combatants.models.reduce(reduceFunc, 0);
			var name = $scope.name_base;
			if(maybeAppend){
				name += " " + maybeAppend;
			}

			var combatant = {
				'x': x,
				'y': y,
				'name': name,
				'size': $scope.size,
				'color': $scope.color,
				'aura_size': $scope.aura_size,
				'aura_color': $scope.aura_color,
				'layer_id': layerId
			};

			var defer = $rootScope.Combatants.create(combatant);
			defer.then(function(success){
				if(mouseIsDown){
					madeCombatant = success;
				}
			},
			function(fail){
				console.error('could not make combatant', fail);
			});

		};

		def.grid_mouseup = function(){
			mouseIsDown = false;
			madeCombatant = false;
		};

		def.grid_mousemove = function(origin, ev, cellX, cellY){
			if(mouseIsDown && madeCombatant){
				madeCombatant.x = Math.floor(cellX);
				madeCombatant.y = Math.floor(cellY);
				madeCombatant.$save();
			}
		};

		return def;
	};

	$scope.setTool = function(){
		Tool(makeAddCombatantTool);
	};
});

Controllers.controller("AddZoneToolCtrl", function($scope, $rootScope, Tool){
	$scope.name = "Blocking Terrain";
	$scope.shape = 'rect';
	$scope.shapes = ['rect', 'circle', 'polyline', 'polygon'];
	$scope.fill_color = '#008800';
	$scope.fill_opacity = 1;
	$scope.stroke_color = '#000000';
	$scope.stroke_width = 5;
	$scope.stroke_opacity = '1';

	$scope.$on('grid_toolchange', function(ev, newTool){
		$scope.toolName = newTool.name;
	});

	$scope.setTool = function(){
		var makeAddZoneTool = function(def){
			def.name = 'Add Zone';
			def.grid_mousedown = function(origin, ev, cellX, cellY){

				var layer = false;
				if($rootScope.Layers.models.selected){
					layer = $rootScope.Layers.models.selected;
				} else {
					layer = $rootScope.Layers.models.reduceRight(function(red, layer){
						if(layer.visible){
							return red || layer;
						}
					}, layer)
				}

				if(! layer){
					console.error("no layer found to put zone on", layer);
					return;
				}

				var regEx = RegExp("^" + $scope.name + "( \\d+)?$");
				var reduceFunc = function(red, zone){
					if(zone.layer_id != layer.id){
						return red;
					}
					var gotNum = 1;
					var match = regEx.exec(zone.name);
					if(match){
						if(match[1] == null){
							gotNum = 1;
						} else {
							gotNum = parseInt(match[1], 10);
						}
						if(gotNum < red){
							return red;
						}
						return gotNum + 1;
					}
					return red;
				}
				var maybeAppend = $rootScope.Zones.models.reduce(reduceFunc, 0);
				var name = $scope.name;
				if(maybeAppend){
					name += " " + maybeAppend;
				}

				var newZone = {
					name:name,
					shape: $scope.shape,
					layer_id: layer.id,
					stroke_color: $scope.stroke_color,
					stroke_width: $scope.stroke_width,
					stroke_opacity: $scope.stroke_opacity,
					fill_color: $scope.fill_color,
					fill_opacity: $scope.fill_opacity
				};

				switch($scope.shape){
					case 'rect':
						newZone.x = $rootScope.nearest(cellX, 2);
						newZone.y = $rootScope.nearest(cellY, 2);
						newZone.width = 1;
						newZone.height = 1;
						break;
					case 'circle':
						newZone.cx = $rootScope.nearest(cellX, 2);
						newZone.cy = $rootScope.nearest(cellY, 2);
						newZone.r = 0.5;
						break;
					case 'polygon':
					case 'polyline':
						var nearX = $rootScope.nearest(cellX, 2);
						var nearY = $rootScope.nearest(cellY, 2);
						newZone.points = [
							{x: nearX, y: nearY + 1},
							{x: nearX, y: nearY},
							{x: nearX + 1, y: nearY}
						];
						break;
				}

				var defer = $scope.Zones.create(newZone);
				defer.then(function(zone){
					console.log('created new zone', zone);

					// Select the zone, and change our tool
					$rootScope.$broadcast("selectzone", zone);
					$rootScope.$broadcast("settool", 'Normal');
					//$scope.setTool('Normal');
				},
				function(fail){
					console.log('could not create new zone', fail);
				});
			}
			return def;
		}
		Tool(makeAddZoneTool);
	};

});

Controllers.controller("EditZoneCtrl", function($scope, $rootScope, Tool){
	$scope.zone = null;

	$scope.setPoints = function(){ return; };

	$scope.$watch('Zones.models.selected', function(newVal){
		$scope.zone = newVal;
		if(! $scope.zone){
			$scope.points = [];
			return;
		}

		var zone = $scope.zone;
		$scope.setPoints(zone);
	});

	$scope.$on('zone_moved', function(ev, zone){
		if($scope.zone && zone == $scope.zone){
			$scope.setPoints(zone);
		}
	});

});

Controllers.controller("EditPolyZoneCtrl", function($scope, $rootScope, Tool){
	$scope.points = [];
	$scope.midPoints = [];

	$scope.$watch('Zones.models.selected', function(zone){
		if(! zone){
			$scope.points = [];
			return;
		}
		$scope.setPoints(zone);
		$scope.setMidPoints(zone);
	});

	$scope.setPoints = function(zone){
		$scope.points = zone.points.map(function(p){
			return {x: p.x, y: p.y};
		});
	};

	$scope.setMidPoints = function(zone){
		$scope.midPoints = zone.points.map(function(p, ind, arr){
			var ind = ind - 1;
			if(ind === -1){
				ind = arr.length - 1;
			}
			var x = (p.x + arr[ind].x) / 2;
			var y = (p.y + arr[ind].y) / 2;
			return {'x':x, 'y':y};
		});
	}

	$scope.addPoint = function(point, ev){
		var ind = $scope.midPoints.indexOf(point);
		point.x = $rootScope.nearest(point.x, 2);
		point.y = $rootScope.nearest(point.y, 2);
		$scope.zone.points.splice(ind, 0, point);
		$scope.points.splice(ind, 0, point);
		$scope.setMidPoints($scope.zone);

		var moveFunc = function(origin, ev, cellX, cellY){
			var nearX = $rootScope.nearest(cellX, 2);
			var nearY = $rootScope.nearest(cellY, 2);
			$scope.points[ind].x = nearX;
			$scope.points[ind].y = nearY;
			$scope.zone.points[ind].x = nearX;
			$scope.zone.points[ind].y = nearY;
			$scope.setMidPoints($scope.zone);
			$scope.zone.$save();
		};

		var override = {
			'grid_mousemove': moveFunc
		};

		ev.overrideTool = override;
	};

	$scope.movingPoint = function(point, ev){
		var ind = $scope.points.indexOf(point);

		var moveFunc = function(origin, ev, cellX, cellY){
			var nearX = $rootScope.nearest(cellX, 2);
			var nearY = $rootScope.nearest(cellY, 2);
			$scope.points[ind].x = nearX;
			$scope.points[ind].y = nearY;
			$scope.zone.points[ind].x = nearX;
			$scope.zone.points[ind].y = nearY;
			$scope.setMidPoints($scope.zone);
			$scope.zone.$save();
		};

		var override = {
			'grid_mousemove': moveFunc
		};

		ev.overrideTool = override;
	};

	var pointToRm = -1
	$scope.setPointToRemove = function(point, ev){
		$rootScope.stopPropagation(ev);
		if($scope.points.length > 2){
			pointToRm = $scope.points.indexOf(point);
			return;
		}
		pointToRm = -1;
	};

	$scope.removePoint = function(point, ev){
		$rootScope.stopPropagation(ev);
		var ind = $scope.points.indexOf(point);
		if(! (ind === pointToRm) ){
			pointToRm = -1;
			return;
		}
		$scope.zone.points.splice(ind, 1);
		$scope.setPoints($scope.zone);
		$scope.setMidPoints($scope.zone);
		$scope.zone.$save();
		pointToRm = -1;
	};

});

Controllers.controller("EditCircleZoneCtrl", function($scope, $rootScope, Tool){
	$scope.points = [];

	$scope.$watch('Zones.models.selected', function(zone){
		if(! zone){
			$scope.points = [];
			return
		}
		$scope.setPoints(zone);
	});

	$scope.$on('zone_moved', function(ev, zone){
		if($scope.zone && zone == $scope.zone){
			$scope.setPoints(zone);
		}
	});

	$scope.setPoints = function(zone){
		$scope.points = [
			{x: zone.cx - zone.r, y: zone.cy},
			{x: zone.cx, y: zone.cy - zone.r},
			{x: zone.cx + zone.r, y: zone.cy},
			{x: zone.cx, y: zone.cy + zone.r}
		];
	};

	$scope.movingPoint = function(point, ev){

		var syncFunc = function(origin, ev, cellX, cellY){
			var r = $rootScope.nearest(Math.abs($scope.zone.cy - cellY), 2);
			$scope.zone.r = r;
			$scope.zone.$save();
			$scope.setPoints($scope.zone);
		};

		if($scope.zone.cy == point.y){
			syncFunc = function(origin, ev, cellX, cellY){
				var r = $rootScope.nearest(Math.abs($scope.zone.cx - cellX), 2);
				$scope.zone.r = r;
				$scope.zone.$save();
				$scope.setPoints($scope.zone);
			};
		}

		var override = {
			'grid_mousemove': syncFunc
		};

		ev.overrideTool = override;
	};
});

Controllers.controller("EditRectZoneCtrl", function($scope, $rootScope, Tool){
	$scope.points = [];

	$scope.$watch('Zones.models.selected', function(zone){
		if(! zone){
			$scope.points = [];
			return;
		}

		setPoints(zone);
	});

	var setPoints = function(zone){
		$scope.points = [
			{x: zone.x, y:zone.y},
			{x: zone.x + zone.width, y:zone.y},
			{x: zone.x, y:zone.y + zone.height},
			{x: zone.x + zone.width, y:zone.y + zone.height}
		];
	};

	$scope.$on('zone_moved', function(ev, zone){
		if($scope.zone && zone == $scope.zone){
			setPoints(zone);
		}
	});

	$scope.movingPoint = function(point, ev){
		console.log('moving point!', point, ev);
		var movingPoint = point;
		var points = $scope.points;
		var anchored = null;
		var syncX = null;
		var syncY = null;
		var indexOf = $scope.points.indexOf(point);
		switch(indexOf){
			case 0:
				anchored = points[3];
				syncX = points[2];
				syncY = points[1];
				break;
			case 1:
				anchored = points[2];
				syncX = points[3];
				syncY = points[0];
				break;
			case 2:
				anchored = points[1];
				syncX = points[0];
				syncY = points[3];
				break;
			case 3:
				anchored = points[0];
				syncX = points[1];
				syncY = points[2];
		}

		var resizeZone = function(){
			console.log('resizing zone', movingPoint, anchored);
			var width = Math.abs(movingPoint.x - anchored.x);
			var height = Math.abs(movingPoint.y - anchored.y);
			var x = movingPoint.x < anchored.x ? movingPoint.x : anchored.x;
			var y = movingPoint.y < anchored.y ? movingPoint.y : anchored.y;
			$scope.zone.x = x;
			$scope.zone.y = y;
			$scope.zone.width = width;
			$scope.zone.height = height;
			$scope.zone.$save();
			syncX.x = movingPoint.x;
			syncY.y = movingPoint.y;
		};

		var override = {
			'grid_mousemove': function(origin, ev, cellX, cellY){
				movingPoint.x = $rootScope.nearest(cellX, 2);
				movingPoint.y = $rootScope.nearest(cellY, 2);
				resizeZone();
			}
		};
		if(ev.overrideTool){
			return;
		}
		ev.overrideTool = override;
	}
});

Controllers.controller("MapToolsCtrl", function($scope, $rootScope, Tool){
	$scope.currentTool = Tool.currentTool;

	$scope.$on('grid_toolchange', function(ev, newTool){
		$scope.currentTool = newTool;
	});

	$scope.$on('settool', function(ev, toolName){
		$scope.setTool(toolName);
	});

	var makePanTool = function(def){
		var dragData = {};
		var mouseDown = false;
		var overrideTool = false;
		def.name = 'Pan Map';

		def.grid_mousedown = function(origin, ev, cellX, cellY){
			mouseDown = true;
			if(ev.overrideTool){
				overrideTool = ev.overrideTool;
			}
			if(overrideTool.grid_mousedown){
				overrideTool.grid_mousedown(origin, ev, cellX, cellY);
				return;
			}
			if(ev.overrideTool){
				return;
			}

			dragData.lastX = ev.pageX;
			dragData.lastY = ev.pageY;
			mouseDown = true;
			overrideTool = false;
			//console.log('pan map mousedown', ev.combatant, origin, ev);
		};
		def.grid_mouseup = function(origin, ev){
			if(overrideTool && overrideTool.grid_mouseup){
				overrideTool.grid_mouseup(origin, ev);
			}
			mouseDown = false;
			overrideTool = false;
		};
		def.grid_mousemove = function(origin, ev, cellX, cellY){
			if(! mouseDown){
				return;
			}
			if(overrideTool && overrideTool.grid_mousemove){
				overrideTool.grid_mousemove(origin, ev, cellX, cellY);
				return;
			}
			if(! mouseDown){
				return;
			}
			var deltaX = ev.pageX - dragData.lastX;
			var deltaY = ev.pageY - dragData.lastY;
			dragData.lastX = event.pageX;
			dragData.lastY = event.pageY;
			$scope.translate.x = $scope.translate.x + deltaX;
			$scope.translate.y = $scope.translate.y + deltaY;
		};
		def.deselected = function(){
			overrideTool = false;
			mouseDown = false;
		}
		return def;
	};

	var makeMeasureTool = function(def){
		def.start = {};
		def.stop = {};
		def.measuring = false;
		def.name = 'Measure';
		def.normalize = function(n){
			return Math.floor(n);
		};
		def.distance = function(){
			var deltaX = Math.abs(this.start.x - this.stop.x);
			var deltaY = Math.abs(this.start.y - this.stop.y);
			var out = deltaX > deltaY ? deltaX : deltaY;
			return out;
		};
		def.deselected = function(){
			this.measuring = false;
		};
		def.grid_mousedown = function(origin, ev, cellX, cellY){
			if(this.measuring){
				this.measuring = false;
				return;
			}
			this.measuring = true;
			var normalX = this.normalize(cellX);
			var normalY = this.normalize(cellY);
			this.start = {x: normalX, y: normalY};
			this.stop = {x: normalX, y: normalY};
		};
		def.grid_mousemove = function(origin, ev, cellX, cellY){
			if(this.measuring){
				var normalX = this.normalize(cellX);
				var normalY = this.normalize(cellY);
				this.stop.x = normalX;
				this.stop.y = normalY;
			}
		};
		return def;
	};

	$scope.setTool = function(tool){
		switch(tool){

			case 'measure':
				Tool(makeMeasureTool);
				break;

			default:
				Tool(makePanTool);
		}
	};

	$scope.setTool('Normal');

});

Controllers.controller("MeasureToolCtrl", function($scope, $rootScope, Tool){

	$rootScope.$on('grid_toolchange', function(ev, newTool){
		$scope.toolData = newTool;
	});

	var safeToolData = function(){
		var args = Array.prototype.slice.call(arguments, 0);
		var def = args.shift();

		var reduceFunc = function(red, prop){
			return red[prop] || false;
		};

		var reduced = args.reduce(reduceFunc, $scope.toolData);
		return reduced || def;
	};

	$scope.mid = function(prop){
		var n1 = safeToolData(0, 'start', prop);
		var n2 = safeToolData(0, 'stop', prop);
		return (n1 + n2) / 2;
	};

	$scope.midx = function(){
		return $scope.mid('x');
	};

	$scope.midy = function(){
		return $scope.mid('y');
	};

	$scope.startX = function(){
		var x = safeToolData(0, 'start', 'x') + 0.5;
		return $scope.transformX(x * 32);
	};

	$scope.startY = function(){
		var y = safeToolData(0, 'start', 'y') + 0.5;
		return $scope.transformY(y * 32);
	}

	$scope.stopX = function(){
		var x = safeToolData(0, 'stop', 'x') + 0.5;
		return $scope.transformX(x * 32);
	};

	$scope.stopY = function(argument) {
		var y = safeToolData(0, 'stop', 'y') + 0.5;
		return $scope.transformY(y * 32);
	};

	$scope.distance = function(){
		if($scope.toolData.start){
			return $scope.toolData.distance();
		}
		return 0;
	};

});

Controllers.controller("EditMapCtrl", function($scope, $rootScope) {
	//FIXME: For demo, only
	$scope.map = ($rootScope.maps && $rootScope.maps[0]) || {};

	$scope.buttons = [
		{ name: 'Layers', icon: 'icon-map-marker', menu: [] },
		{ name: 'Zones & Auras', icon: 'icon-map-marker', menu: [] },
		{ name: 'Combatants', icon: 'icon-hand-up', menu: [] },
		{ name: 'New Combatant', icons: ['icon-plus icon-small', 'icon-user'], class: "btn-primary" }
	];

	$scope.tools = [
		{ name: 'Pan', icon: 'icon-fullscreen' }
	];
});

//----------------------------------------------------------------------------------------------------------------------

Controllers.controller("HeaderCtrl", function($scope) {
	$scope.topBarToggle = 'open';

	// Handles the case of the collapse button being clicked, so we can pick up on the fact that we need to change the
	// icon. Seems the best way to handle this.
	$scope.handleToggle = function() {
		$scope.topBarToggle = $scope.getCollapseState();
	};

	// Gets the current state of the top bar; either open, or closed.
	$scope.getCollapseState = function() {
		var topBar = document.querySelector("#top-bar");

		if(! $(topBar).hasClass('in')) {
			return 'open';
		} else {
			return 'closed';
		}
	};

	$scope.logit = function(){
		console.log('der thangs', arguments);
	}
});

Controllers.controller("GridCtrl", function($scope, $rootScope, MapSocket) {
	var header = $("#main-header");
	var topBar = $("#top-bar");
	var docElem = $(window);
	var CELL_SIZE = 32;

	function calcGridheight(buffer) {
		buffer = buffer || header.height();
		//buffer = buffer || 0;
		var height = docElem.height() - buffer - 25;
		return height;
	}

	// Catch window resize events
	docElem.bind('resize', function(){
		$scope.$apply(function(){
			$scope.gridHeight = calcGridheight();
		});
	});

	// Catch header shown events.
	topBar.on('hidden', function(){
		$scope.$apply(function(){
			$scope.gridHeight = calcGridheight();
		});
	});

	// Catch header shown events.
	topBar.on('shown', function(){
		$scope.$apply(function(){
			$scope.gridHeight = calcGridheight();
		});
	});

	$scope.gridHeight = calcGridheight(82);

	$rootScope.translate = {x: 0, y: 0};
	$rootScope.scale = 1;

	var dragData = {
		panning: false
	};

	$scope.mouseDown = function(ev){
		xy = $rootScope.pixelsToCells(ev.layerX, ev.layerY),
		$rootScope.$broadcast('grid_mousedown', ev, xy[0], xy[1]);
	};

	$scope.mouseUp = function(ev){
		xy = $rootScope.pixelsToCells(ev.layerX, ev.layerY);
		$rootScope.$broadcast('grid_mouseup', ev, xy[0], xy[1]);
	};

	$scope.mouseMove = function(ev){
		xy = $rootScope.pixelsToCells(ev.layerX, ev.layerY);
		$rootScope.$broadcast('grid_mousemove', ev, xy[0], xy[1]);
	};

	$scope.zoom = function(ev){
		//console.log('der zoom', ev, arguments);
		var inc = 0.1
		var delta = ev.deltaY > 0 ? inc : (-1 * inc);
		var newZoom = $rootScope.scale - delta;
		$rootScope.scale += delta;
		if(newZoom < 0.1){
			newZoom = 0.1;
		} else if(newZoom > 3){
			newZoom = 3
		}
		$rootScope.scale = newZoom;
		ev.preventDefault();
		return false;
	}
});

//----------------------------------------------------------------------------------------------------------------------
