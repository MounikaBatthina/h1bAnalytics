var app = angular.module('ToDoListApp');

app.controller("HomeController",function($scope){
    console.log("inside home controller");

    $scope.addTask = function () {
        console.log("Inside add task");
        console.log($scope.taskToAdd);
    }
});