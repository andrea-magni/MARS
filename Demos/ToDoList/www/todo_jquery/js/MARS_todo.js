token_url = "/api/todo/token";

function init_ajax(){	
	$.ajaxSetup({
	  headers: { 
	    Accept : "application/json"
	  },
	});
}

function refresh_token( data ){
	if (data.Authenticated == true) {
		$('#loginDiv').hide();
		after_login();		
	} else {
		$('#loginDiv').show();
	}
	$('span#userName').text(data.UserName);
}

function get_token(){
	$.get( token_url, function (data) {
		refresh_token(data);
	});
}

function setup_login(){
	$('#loginForm').submit( function ( event ){
		event.preventDefault();

		var $form = $( this ),
		user = $form.find( "input[name='user']" ).val(),
		pass = $form.find( "input[name='pwd']" ).val();

		// Send the data using post
		var posting = $.post( token_url, { username : user, password: pass } );

		// Put the results in a div
		posting.done(function( data ) {
			refresh_token(data);
		});          
	});
}

function logout(){
	$.ajax({
	    url: token_url,
	    type: 'DELETE',
	    success: function(result) {
	    	$('#loginDiv').show();
	    	$('span#userName').text('N/A');
	    	after_logout();
	    }
	});

}

function after_login(){
	getList();
}

function after_logout(){
	$('#todo-list').empty();
}

function getList(){
	$.getJSON("/api/todo/item/",
		function (data){
			$('#todo-list').empty();
			$.each(data, function (index, item) {
				$('#todo-list').append(
					'<div class="todo-item">'
						+'<div class="text">' + item.Text + '</div>'
						+'<div class="details">'
							+'<div class="owner">' + item.Owner + '</div>'
							+'<div class="id">' + item.ID + '</div>'
						+'</div>'
					+'</div>'
				);
			});
		}
	);
}
