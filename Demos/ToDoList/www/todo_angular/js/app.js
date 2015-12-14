(function(){
	var app = angular.module('todo', ['ngRoute', 'todo-directives', 'todo-services'])
	.config([
		'$routeProvider'
		, function($routeProvider){
			$routeProvider
			.when('/', {
				templateUrl: 'views/home.html'
			})
			.when('/login', {
				templateUrl: 'views/login.html'
			})
			.otherwise({
				redirectTo: '/'
			});

		}
	]);

	app.controller('LoginController', ['$http', '$location', '$route', 'tokenService', function($http, $location, $route, tokenService){
		var login = this;
		login.userName = "";
		login.password = "";

		login.token = "";
		login.isAuthenticated = function (){
			return tokenService.isAuthenticated();
		};

		login.getUserName = function(){
			return tokenService.getUserName();
		}

		/* token manipolation */

		login.getToken = function(){
			tokenService.getToken();
		};

		login.login = function(){
			tokenService.login(login.userName, login.password
				, function () {
					if (tokenService.isAuthenticated()) {
						login.userName = tokenService.getUserName();
						$route.reload();
					}
				}
			);
		};

		login.logout = function(){
			tokenService.logout(
				function () {
					$route.reload();
				}
			);
		};

		tokenService.getToken();

	}])

	app.controller('TodoController', ['$http', '$location', 'tokenService', function($http, $location, tokenService){
		var todo = this;
		todo.items = [];
		
		todo.canInsert = true;
		todo.isEditing = false;

		todo.isAuthenticated = function (){
			return tokenService.isAuthenticated();
		};

		todo.getAll = function(){
			$http.get('/rest/todo/item/').success(function(data){
				todo.items = data;
				todo.canInsert = true;
			}).error(
				function(data){
					todo.items = [];
					todo.canInsert = false;
				}
			);
		};

		todo.deleteItem = function(item){
			$http.delete('/rest/todo/item/' + item.ID).success(function(data){
				todo.items = todo.items.filter(function (el) {
					return el.ID !== item.ID;
				});
			});
		};

		todo.addItem = function(){
			$http({
				method: 'POST',
				url: '/rest/todo/item/',
				data: 'text=' + todo.itemText,
				headers: {'Content-Type': 'application/x-www-form-urlencoded'}
			}).success(function(data){
				todo.items.push(data);
				todo.itemText = "";
			});
		};

		todo.editItem = function(item){
			todo.isEditing = true;
			todo.editingItem = item;
		};

		todo.updateItem = function(item){
			$http({
				method: 'PUT',
				url: '/rest/todo/item/' + item.ID,
				data: 'text=' + todo.editingItem.Text,
				headers: {'Content-Type': 'application/x-www-form-urlencoded'}
			}).success(function(data){
				todo.isEditing = false;
				todo.editingItem = null;
			});			
		};

		//tokenService.getToken(function(){
			if (tokenService.isAuthenticated()) {
				todo.getAll();			
			}
		//});
	
	}]);

}) ();