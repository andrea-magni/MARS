const MODE_DIAGNOSTICS = 'diagnostics';
const MODE_RESOURCES = 'resources';

var current_mode = MODE_DIAGNOSTICS;
var interval_id;

function set_mode_diagnostics(){

	$('.resources').hide();
	$('.diagnostics').show();

	update_diagnostics();

	clearInterval(interval_id);
	interval_id = setInterval(update_diagnostics, 1000);

	current_mode = MODE_DIAGNOSTICS;
}

function set_mode_resources(){

	$('.resources').show();
	$('.diagnostics').hide();

	update_resources();

	clearInterval(interval_id);
	//interval_id = setInterval(update_resources, 1000);

	current_mode = MODE_RESOURCES;
}


function update_diagnostics(){
	$.getJSON("/api/diagnostics/manager/",
		function (data){
			var keys = Object.keys(data.engine);
			var enginename = keys[0];
			var stat = data.engine[enginename];
			$('.engineRequestCount').text(stat.RequestCount);
			$('.engineLastRequestTime').text(stat.LastRequestTime);
			$('.engineSessionCount').text(stat.SessionCount);
			$('.engineActiveSessionCount').text(stat.ActiveSessionCount);
			$('.engineLastSessionStart').text(stat.LastSessionStart);
			$('.engineLastSessionEnd').text(stat.LastSessionEnd);

			var table_body = $('.app_table');
			$.each(data.apps, 
				function (i, item) {
					var keys = Object.keys(item);
					var appname = keys[0];
					var stat = item[keys[0]];
					var tr = table_body.find('#'+appname);
					if (tr.length) {
						tr.find('.RequestCount').text(stat.RequestCount);
						tr.find('.AverageTimePerRequest').text(stat.AverageTimePerRequest);
						tr.find('.LastRequestTime').text(stat.LastRequestTime);
					} else {
						table_body.append(
							  '<tr id="' + appname + '">'
							+   '<td class="name">' + appname + '</td>'
							+   '<td class="RequestCount text-center">' + stat.RequestCount + '</td>'
							+   '<td class="AverageTimePerRequest text-center">' + stat.AverageTimePerRequest + '</td>'							
							+   '<td class="LastRequestTime">' + stat.LastRequestTime + '</td>'
							+ '</tr>');
					}

				
				}
			);


			var table_body = $('.session_table');
			table_body.html('');
			$.each(data.sessions, 
				function (i, item) {
					table_body.append(
						  '<tr id="' + item.Token + '">'
							+ '<tr class="main">'  
							+   '<td class="Token">' + item.Token + '</td>'
							+   '<td class="StartTime">' + item.StartTime + '</td>'						
							+   '<td class="UserName text-center">' + (item.UserName ? item.UserName : '') + '</td>'
							+   '<td class="Authenticated">' + item.Authenticated + '</td>'
							+   '<td class="UserRoles">' + (item.UserRoles ? item.UserRoles : '')  + '</td>'
							+ '</tr>'  
							+ '<tr class="detail" style="background-color: white; font-size: 80%; color: gray;">'  
							+   '<td class="text-right">Last Request:</td>'
							+   '<td class="LastRequest" colspan="4">' + (item.LastRequest ? item.LastRequest : '')  + '</td>'
							+ '</tr>'  
						+ '</tr>');				
				}
			);

		
		}
	);
}

function update_resources(){
	$.getJSON("/api/diagnostics/resources/",
		function (data){
			
			var div = $('div#app_resources');

			div.html('<ul>');

			$.each(data, 
				function (i, item) {
					var keys = Object.keys(item);
					var appname = keys[0];
					var resources = item[appname];

					var resources_list = '<ul>';
					$.each(resources, 
						function (k, resource) {
							resources_list = resources_list + '<li>' + resource + '</li>'
						}
					);

					resources_list = resources_list + '</ul>';

					div.append('<li>'+ appname + resources_list + '</li>');

				}
			);

			div.append('<ul>');			
		}
	);
}
