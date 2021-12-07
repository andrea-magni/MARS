let baseURL = '/rest/default/';
let authData = {"IsVerified": false};
let allUsersData = [];

function doError(errorMsg) {
    $('div#error').html(
        `<div class="alert alert-danger alert-dismissible fade show" role="alert">
            <span id="errorMsg"> ${errorMsg} </span>
            <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
        </div>`
    );
    $('div#error').show();
}

function clearError(){
    $('div#error').html('');
    $('div#error').hide();
}

function doLogin(credentials, onSuccess) {
    $.ajax({
        url: `${baseURL}token`,
        type: 'POST',
        data: JSON.stringify(credentials),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: onSuccess,
        error: function (content) { doError(content); }
    });
}

function doLogout(onSuccess) {
    $.ajax({
        url: `${baseURL}token`,
        type: 'DELETE',
        success: onSuccess,
        error: function (content) { doError(content); }
    });
}

function doRetrieveAllUsers(onSuccess){
    $.ajax({
        url: `${baseURL}user`,
        type: 'GET',
        beforeSend: function (xhr) {
            xhr.setRequestHeader('Authorization', `Bearer ${authData.Token}`);
        },
        success: onSuccess,
        error: function (content) { doError(content); }
    });
}

function doRetrieveUser(username, onSuccess){
    $.ajax({
        url: `${baseURL}user/${username}`,
        type: 'GET',
        beforeSend: function (xhr) {
            xhr.setRequestHeader('Authorization', `Bearer ${authData.Token}`);
        },
        success: onSuccess,
        error: function (content) { doError(content); }
    });
}


function doDeleteUser(userData, onSuccess){
    $.ajax({
        url: `${baseURL}user/${userData.username}`,
        type: 'DELETE',
        beforeSend: function (xhr) {
            xhr.setRequestHeader('Authorization', `Bearer ${authData.Token}`);
        },
        success: onSuccess,
        error: function (content) { doError(content); }
    });
}

function doCreateUser(userData, onSuccess) {
    $.ajax({
        url: `${baseURL}user`,
        type: 'POST',
        data: JSON.stringify(userData),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        beforeSend: function (xhr) {
            xhr.setRequestHeader('Authorization', `Bearer ${authData.Token}`);
        },
        success: onSuccess,
        error: function (content) { doError(content); }
    });
}

function updateAuthState(data) {
    authData = data;
    let loggedIn = authData.IsVerified == true;
    if (loggedIn) {
        $('.loggedIn').show();
        $('.notLoggedIn').hide();
        $('.admin').toggle(isAdmin());
        $('.userdata.username').html(authData.UserName);
        $('.userdata.displayName').html(authData.Claims.displayName);
        $('.userdata.isAdmin').html((isAdmin() ? 'Yes' : 'No'));
    } else {
        $('.loggedIn').hide();
        $('.notLoggedIn').show();
        $('.admin').hide();
        $('.userdata.username').html('');
        $('.userdata.displayName').html('');
    }
}

function renderAllUsers(){
    $('#userList').html('');
    $.each(allUsersData, function (index, element) {
        let lastUpdate = new Date(element.lastUpdateAt);
        let created = new Date(element.createdAt);
        $('#userList').append(
            `<li class="list-group-item">
                <div class="d-flex justify-content-between align-items-start">
                
                    <div class="ms-2 me-auto">
                        <div class="fw-bold">${index + 1}. ${element.displayName} (${element.username}) <i class="admin fas fa-users-cog ${ (element.roles || []).includes('admin') ? "" : "collapse" }"></i></div>
                        
                        <div class="badge bg-secondary">Created: ${created.toLocaleDateString()} ${created.toLocaleTimeString()}</div>
                        <div class="badge bg-secondary">Updated: ${lastUpdate.toLocaleDateString()} ${lastUpdate.toLocaleTimeString()}</div>
                    </div>
                    
                    <div class="">
                        <a href="#" class="btn btn-danger deleteUserButton" username="${element.username}">Delete <i class="fas fa-trash-alt"></i></a>
                        <a href="#" class="btn btn-secondary editUserButton" username="${element.username}">Edit <i class="fas fa-edit-alt"></i></a>
                    </div>
                    
                </div>
               
            </li>`
        );
    });

    bindUIElements();
}

function refreshUserList(){
    doRetrieveAllUsers(function (data){
        allUsersData = data;
        renderAllUsers();
    });
}

function isAdmin(){
    return isLoggedIn() && (authData.Roles == 'admin');
}

function isLoggedIn(){
    return (authData.IsVerified);
}

function bindUIElements(){
    clearError();

    $('.form-signin').submit(function (e) {
        let authCredentials = {};
        authCredentials.username = $('#authUsername').val();
        authCredentials.password = $('#authPassword').val();

        doLogin(
            authCredentials
            , function(result) {
                updateAuthState(result);
                if (isAdmin()) { refreshUserList(); }
                (isLoggedIn() ? clearError() : doError('Login failed'));
            }
        );
    });

    $('#loginButton').off('click').bind('click', function(e) {
        let authCredentials = {};
        authCredentials.username = $('#authentication.row input#authUsername').val();
        authCredentials.password = $('#authentication.row input#authPassword').val();

        doLogin(
            authCredentials
            , function(result) {
                updateAuthState(result);
                if (isAdmin()) { refreshUserList(); }
                (isLoggedIn() ? clearError() : doError('Login failed'));
            }
        );
    });

    $('#logoutButton').off('click').bind('click', function(e) {
        doLogout(
            function(result) {
                updateAuthState(result);
                clearError();
            }
        );
    });

    // ------------------------------------------------------------------------------------------------------

    $('#createUserButton').off('click').bind('click', function(e) {
        let userData = {};
        userData.username = $('#createUser input#username').val();
        userData.password = $('#createUser input#password').val();
        userData.displayName = $('#createUser input#displayName').val();
        userData.roles = [$('#createUser select#role').val()];

        doCreateUser(
            userData
            , function(result) {
                refreshUserList();
                clearError();
            }
        );
    });

    $('.deleteUserButton').off('click').bind('click', function(e) {
        let userData = {};
        userData.username = $(this).attr('username');

        doDeleteUser(
            userData
            , function(result) {
                refreshUserList();
                clearError();
            }
        );
    });

    $('.editUserButton').off('click').bind('click', function(e) {
        let userData = {};
        userData.username = $(this).attr('username');
        doRetrieveUser($(this).attr('username'), function(userData){
            $('#createUser input#username').val(userData.username);
            $('#createUser input#password').val('');
            $('#createUser input#displayName').val(userData.displayName);
            $('#createUser select#role').val(userData.roles);

            clearError();
            $('#createUser input#username').focus();
        });

    });


    $('#allUsersButton').off('click').bind('click', function(e) {
        refreshUserList();
    })

}