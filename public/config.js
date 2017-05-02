var app = angular.module('ToDoListApp', ['ui.router', 'ui.bootstrap','ui.bootstrap.collapse']);

app.config(function($stateProvider,$urlRouterProvider,$locationProvider){
    console.log("inside config function");

    $stateProvider
        .state('home',{
            url:'/home',
            templateUrl:'templates/home.html',
            controller:'HomeController'
        })
    $urlRouterProvider.otherwise('/home');
});