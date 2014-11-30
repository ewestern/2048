{-# LANGUAGE OverloadedStrings, CPP, FlexibleInstances #-}
module JavaScript where

import qualified Data.Text as T


#ifdef __GHCJS__
import Control.Applicative
import GHCJS.Types
import GHCJS.Foreign
data JSEvent_
type Event = JSRef JSEvent_

data DomElement_
type DomElement  = JSRef DomElement_

type Element = DomElement

instance Show (JSRef DomElement_) where
	show e = "Element"


foreign import javascript unsafe 
  "$1.keyCode"
  getKey :: Event -> IO Int

foreign import javascript unsafe
  "$1.id;"
  js_getId :: DomElement -> IO JSString

foreign import javascript unsafe 
  "$1.classList.add($2);"
  js_classListAdd :: DomElement -> JSString -> IO ()

foreign import javascript unsafe 
  "$1.classList.remove($2);"
  js_classListRemove :: DomElement -> JSString -> IO ()

foreign import javascript unsafe
  "$1.addEventListener($2, $3);"
  js_addEventListener :: DomElement -> JSString -> (JSFun (Event -> IO ())) -> IO ()

foreign import javascript unsafe
  "window.addEventListener($1, $2);"
  js_addWindowListener :: JSString -> (JSFun (Event -> IO ())) -> IO ()

foreign import javascript unsafe 
  "document.createElement($1)"
  js_createElement :: JSString -> IO DomElement

foreign import javascript unsafe 
  "$1.appendChild($2)"
  js_appendChild :: DomElement -> DomElement -> IO DomElement

foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  js_setAttribute :: DomElement -> JSString -> JSString -> IO ()

foreign import javascript unsafe
  "document.getElementById($1)"
  js_getElementById :: JSString -> IO DomElement

foreign import javascript unsafe
  "$1.removeChild($2)"
  js_removeChild :: DomElement -> DomElement -> IO ()

foreign import javascript unsafe
	"document.getElementsByClassName($1)"
  js_getElementsByClassName :: JSString -> IO (JSArray (DomElement_))

getId :: DomElement -> IO T.Text
getId el = fromJSString <$> js_getId el

removeChild :: DomElement -> DomElement -> IO ()
removeChild = js_removeChild

getElementById :: T.Text -> IO DomElement
getElementById = js_getElementById . toJSString 

getElementsByClassName :: T.Text -> IO [DomElement]
getElementsByClassName c = do
  arr <- js_getElementsByClassName $ toJSString c
  ls <- fromArray arr
  return ls
 	

setAttribute :: DomElement -> T.Text -> T.Text -> IO ()
setAttribute el att val = js_setAttribute el (toJSString att) (toJSString val)

appendChild :: DomElement -> DomElement -> IO DomElement
appendChild parent child = js_appendChild parent child

addClass :: DomElement -> T.Text -> IO DomElement
addClass el cl = (js_classListAdd el $ toJSString cl) >> return el

removeClass :: DomElement -> T.Text -> IO ()
removeClass el cl = js_classListRemove el $ toJSString cl

createElement :: T.Text -> IO DomElement
createElement = js_createElement . toJSString

makeCallback :: (Event -> IO ()) -> IO (JSFun (Event -> IO ()))
makeCallback hcb = asyncCallback1 AlwaysRetain hcb

addEventListener :: DomElement -> T.Text -> (Event -> IO()) -> IO ()
addEventListener el s cb = makeCallback cb >>= js_addEventListener el (toJSString s)

addWindowListener :: T.Text -> (Event -> IO ()) -> IO ()
addWindowListener s cb = makeCallback cb >>= js_addWindowListener (toJSString s)

createElementWithClass :: T.Text -> T.Text -> IO (DomElement)
createElementWithClass tag klass = createElement tag >>= (\e -> addClass e klass)

createElementWithClassAndId :: T.Text -> T.Text -> T.Text -> IO (DomElement)
createElementWithClassAndId tag klass iD =  do
  de <- createElementWithClass tag klass
  setAttribute de "id" iD
  return de

#else
type Element = String
getId = error "Only available in javascript" 
removeChild = error "Only available in javascript" 
getElementById = error "Only available in javascript" 
getElementsByClassName = error "Only available in javascript" 
setAttribute = error "Only available in javascript"  
appendChild = error "Only available in javascript" 
addClass = error "Only available in javascript" 
removeClass  = error "Only available in javascript" 
createElement = error "Only available in javascript" 
makeCallback  = error "Only available in javascript" 
addEventListener = error "Only available in javascript" 
addWindowListener = error "Only available in javascript" 
createElementWithClass = error "Only available in javascript" 
createElementWithClassAndId = error "Only available in javascript" 
#endif
