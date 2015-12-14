(function(){
	var app = angular.module('todo-directives', []);

	app.directive('userInfo', function(){
		return {
			restrict: 'E',
			templateUrl: 'user-info.html'
		}
	});

	app.directive('loginForm', function(){
		return {
			restrict: 'E',
			templateUrl: 'login-form.html'
		}
	});

	app.directive('todoItemsList', function(){
		return {
			restrict: 'E',
			templateUrl: 'todo-items-list.html'
			, controller: 'TodoController'
			, controllerAs: 'todo'
		}
	});

	app.directive('addItemForm', function(){
		return {
			restrict: 'E',
			templateUrl: 'add-item-form.html'
		}
	});

	app.directive('editItemForm', function(){
		return {
			restrict: 'E',
			templateUrl: 'edit-item-form.html'
		}
	});

}) ();