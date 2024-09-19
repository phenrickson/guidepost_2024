targets::tar_load_globals()

upload_report = function(file,
                         name,
                         prefix = "docs/",
                         bucket = "cfb_dev",
                         predefinedAcl = 'bucketLevel',
                         type = 'text/html') {
        
        name = paste0(prefix, name)
        googleCloudStorageR::gcs_upload(file = file,
                                        name = name,
                                        bucket = bucket,
                                        type = type,
                                        predefinedAcl = predefinedAcl)
        
        name
}

upload_report(file = 'docs/predicting_games.html',
              name = 'game_predictions')
