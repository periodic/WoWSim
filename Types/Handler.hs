module Types.Handler where

import Types.Common

import Data.Map

emptyHandlerList = empty

addHandlerToList :: HandlerId -> Handler -> HandlerList -> HandlerList
addHandlerToList = insert

removeHandlerFromList :: HandlerId -> HandlerList -> HandlerList
removeHandlerFromList = delete
