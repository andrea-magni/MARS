let baseURL = 'http://localhost:8080/rest/default/';
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
        data: credentials,
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
        $('.userdata.isAdmin').html((isAdmin() ? 'Yes' : 'No'));
    } else {
        $('.loggedIn').hide();
        $('.notLoggedIn').show();
        $('.admin').hide();
        $('.userdata.username').html('N/A');
    }
}

function renderAllUsers(){
    $('#userList').html('');
    $.each(allUsersData, function (index, element) {
        $('#userList').append(
            `<li class="list-group-item d-flex justify-content-between align-items-start">
                <div class="ms-2 me-auto">
                    <div class="fw-bold">${element.username}</div>
                    ${element.displayName}
                </div>
                <span class="badge bg-primary rounded-pill">${element.lastUpdateAt}</span>
            </li>`
        );
    })
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

    $('#loginButton').bind('click', function(e) {
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

    $('#logoutButton').bind('click', function(e) {
        doLogout(
            function(result) {
                updateAuthState(result);
                clearError();
            }
        );
    });

    // ------------------------------------------------------------------------------------------------------

    $('#createUserButton').bind('click', function(e) {
        let userData = {};
        userData.username = $('#createUser input#username').val();
        userData.password = $('#createUser input#password').val();
        userData.displayName = $('#createUser input#displayName').val();

        doCreateUser(
            userData
            , function(result) {
                refreshUserList();
                clearError();
            }
        );
    });

    $('#deleteUserButton').bind('click', function(e) {
        let userData = {};
        userData.username = $('#deleteUser input#username').val();

        doDeleteUser(
            userData
            , function(result) {
                refreshUserList();
                clearError();
            }
        );
    });

    $('#allUsersButton').bind('click', function(e) {
        refreshUserList();
    })

}