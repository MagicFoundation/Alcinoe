app.controller('ModalUploadAttachmentCtrl', function($scope, $modalInstance, emAuth, grfFiles, editItem, itemType, synConn){

	$scope.files = grfFiles.items;

	var uploadType;
	switch ( itemType ) {
		case 'template':
			uploadType = 'templates';
			break;
		case 'draft':
			uploadType = 'drafts';
			break;
	};


	grfFiles.attrs.action = synConn.getBaseURL()+'wp/'+emAuth.get('workplace').ID+'/settings/UploadContent?'+[
		'GUID='+editItem.Guid,
		'type='+uploadType,
		'folder=attachment'
	].join('&');

    $scope.close = function () {
        var fileNames = [];
        angular.forEach($scope.files, function(f){
            fileNames.push(f.fileName);
        })
        $modalInstance.dismiss(fileNames);
    };

    $scope.setFiles = function(element) {
        $scope.$apply(function($scope) {
          // Turn the FileList object into  Array
            $scope.files = [];
            for (var i = 0; i < element.files.length; i++) {
              $scope.files.push(element.files[i])
            }
          $scope.progressVisible = false
        });
    };

    $scope.uploadFile = function() {
        var fd = new FormData()
        for (var i in $scope.files) {
            fd.append("uploadedFile", $scope.files[i])
        }        
        fd.append('path', $scope.upload.path);
        var xhr = new XMLHttpRequest()
        xhr.upload.addEventListener("progress", uploadProgress, false)
        xhr.addEventListener("load", uploadComplete, false)
        xhr.addEventListener("error", uploadFailed, false)
        xhr.addEventListener("abort", uploadCanceled, false)
        xhr.open('PUT', '/wp/'+emAuth.workplace.ID+'/settings/UploadContent')
        $scope.progressVisible = true
        xhr.send(fd)
    }

    function uploadProgress(evt) {
        $scope.$apply(function(){
            if (evt.lengthComputable) {
                $scope.progress = Math.round(evt.loaded * 100 / evt.total)
            } else {
                $scope.progress = 'unable to compute'
            }
        })
    }

    function uploadComplete(evt) {
        /* This event is raised when the server send back a response */
        $modalInstance.close(evt.target.responseText);
    }

    function uploadFailed(evt) {
        alert("There was an error attempting to upload the file.")
    }

    function uploadCanceled(evt) {
        $scope.$apply(function(){
            $scope.progressVisible = false
        })
        alert("The upload has been canceled by the user or the browser dropped the connection.")
    }

});

app.controller('CommonEditorController', function($scope, $modal){
	$scope.addCommonAtt = function() {

		var modalInstance = $modal.open({
			templateUrl: 'partials/modal-upload-attachment.html',
			controller: 'ModalUploadAttachmentCtrl',
			resolve: {
				itemType: function(){
					return $scope.itemType;
				},
				editItem: function(){
					return $scope.item;
				}
			}
		});

		modalInstance.result.then(function (rel) {
			var r = angular.copy(rel);
		}, function (uploadedFiles) {
            angular.forEach(uploadedFiles, function(fn){
                $scope.item.addAttachment(fn);
            });
		});		
	}
});