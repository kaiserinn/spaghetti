data UpdatedPasta = UpdatedPasta
  { _id :: Maybe Int,
    title :: Maybe String,
    content :: Maybe String,
    slug :: Maybe String,
    view_key :: Maybe String,
    edit_key :: Maybe String,
    updated_view_key :: Maybe String,
    updated_edit_key :: Maybe String
  }
  deriving (Show, Generic)

instance ToJSON UpdatedPasta
instance FromJSON UpdatedPasta

