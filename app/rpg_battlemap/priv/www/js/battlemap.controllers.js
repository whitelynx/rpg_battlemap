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
	console.log('layers list ctrl', $scope, $scope.map.id);

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

	$scope.removeZone = function(zone){
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
		var delPromise = Map.delete({mapid: map.id});
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
	}
});

Controllers.controller("AddCombatantToolCtrl", function($scope, $rootScope, Tool){
	$scope.$on('grid_toolchange', function(ev, newTool){
		$scope.toolName = newTool.name;
	});

	$scope.name_base = 'Fight Man';
	$scope.size = 1;
	$scope.color = 'green';
	$scope.aura_size = 0;
	$scope.aura_color = 'green';

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
	$scope.shapes = ['rect'];
	$scope.fill_color = 'green';
	$scope.fill_opacity = 1;
	$scope.stroke_color = 'black';
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

				if($scope.shape == 'rect'){
					newZone.x = $rootScope.nearest(cellX, 2);
					newZone.y = $rootScope.nearest(cellY, 2);
					newZone.width = 1;
					newZone.height = 1;
				}
				var defer = $scope.Zones.create(newZone);
				defer.then(function(success){
					console.log('created new zone', success);
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

Controllers.controller("MapToolsCtrl", function($scope, $rootScope, Tool){
	$scope.currentTool = Tool.currentTool;

	$scope.$on('grid_toolchange', function(ev, newTool){
		$scope.currentTool = newTool;
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

			dragData.lastX = ev.pageX;
			dragData.lastY = ev.pageY;
			mouseDown = true;
			overrideTool = false;
			console.log('pan map mousedown', ev.combatant, origin, ev);
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
		var newZoom = $rootScope.scale - ev.deltaY
		$rootScope.scale += ev.deltaY;
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
