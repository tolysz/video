import Francium
import Francium.Hooks
import VirtualDom

main :: IO ()
main = react counterApp

counterApp :: Frameworks t => Moment t (Behavior t HTML)
counterApp =
  do
     -- We register two hooks to observe the users interaction with our
     -- application. 'newClickHook' provides us with an event that occurs whenever
     -- the target element is clicked, and a hook that we can bind to clickable
     -- elements.
     (increment,incrHook) <- newClickHook
     (decrement,decrHook) <- newClickHook
     -- We fold over the stream of all increment and decrement events to produce
     -- a single time varying integer. Whenever @increment@ happens, we add 1
     -- and whenever @decrement@ happens we subtract 1.
     let counter =
           accumB (0 :: Int)
                  (unions [fmap (const (+ 1)) increment
                          ,fmap (const (subtract 1)) decrement])
     -- Finally, we produce a time varying HTML fragment. To do so, we transform
     -- the counter value - an 'Int' - into a 'HTML' fragment. We do this by
     -- using 'fmap'. 'fmap' takes a function that views the counter value, and
     -- we use "Francium.HTML" to produce a HTML tree.
     return (fmap (\n ->
                     into body_
                          [into h1_ ["Counter"]
                           -- Here we render the counter, by 'show'ing the value
                           -- of @n@.
                          ,into p_
                                ["The counter currently reads: ",text (show n)]
                           -- Here we build the increment and decrement buttons
                           -- and apply the necessary hooks.
                          ,applyHooks decrHook
                                      (into button "-1")
                          ,"/"
                          ,applyHooks incrHook
                                      (into button "+1")])
                  counter)
