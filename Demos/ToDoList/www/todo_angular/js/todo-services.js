(function(){
	var app = angular.module('todo-services', [])
	.factory("tokenService", function($http){
		var token = {};
		var service = {
			
			isAuthenticated : function(){
				return ((token) && (token.Authenticated == true));
			},

			getUserName : function (){
				if (token.UserName) {
					return token.UserName;
				}
			},

			getToken : function(aThen){
				$http.get('/rest/todo/token/').success(
					function(data){
						token = data;
						if (aThen) aThen(token);
					}
				);
			},

			login : function(aUserName, aPassword, aThen){
				$http({
					method: 'POST',
					url: '/rest/todo/token/',
					data: 'username=' + aUserName + '&password=' + aPassword,
					headers: {'Content-Type': 'application/x-www-form-urlencoded'}
				}).success(
					function(data){
						token = data;
						if (aThen) aThen(token);
					}
				)
				.error(
					function(data){
						token = {};
					}
				);		
			},

			logout : function(aThen){
				$http({
					method: 'DELETE',
					url: '/rest/todo/token/',
				}).success(
					function(data){
						token = {};
						if (aThen) aThen(token);
					}
				).error(
					function(data){
						token = {};
						if (aThen) aThen(token);
					}
				);		
			}

		};

		return service;
	});


}) ();