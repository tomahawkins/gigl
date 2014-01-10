{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Word
import Language.GIGL hiding (GIGL)
import qualified Language.GIGL as G
import Language.GIGL.ACL2

main :: IO ()
main = writeFile "hotel.lisp" $ show $ acl2 "hotel" () hotel

type GIGL = G.GIGL ()

type KeyPair = E (Word64, Word64)

type RoomKey = KeyPair

-- Runs a RoomKey through a lock.  Return a signal if the door unlocks.
doorLock :: Int -> (Word64, Word64) -> RoomKey -> GIGL (E Bool)
doorLock room initLock key = do
  -- Lock state.
  lock <- var ("lock" ++ show room) $ Just initLock
  -- Unlock the door if the key matches the lock state or if the old key value matches the new lock value.
  unlock <- var' ("unlock" ++ show room) $ key .== lock ||| old key .== new lock
  -- Update lock state: if the old key matches the new lock, then use the key as the next lock state, else keep the state the same.
  lock <== mux (old key .== new lock) key lock
  return unlock
  where
  old = ProjFst
  new = ProjSnd

-- Hotel events (event code, (room#, data pair)):
--   checkin     (1, (room#, _))
--   checkout    (2, (room#, _))
--   access room (3, (room#, room key))
type HotelEvent = E (Word64, (Word64, (Word64, Word64)))


-- A hotel with a front desk and two rooms.
hotel :: GIGL ()
hotel = do
  -- Initalize the 2 doors.
  let doorLock1 = doorLock 1 (0, 1)
      doorLock2 = doorLock 2 (0, 2)

  -- Variables to keep track of the current door settings (init to 1 and 2).
  door1 :: E Word64 <- var "door1" $ Just 1
  door2 :: E Word64 <- var "door2" $ Just 2

  -- An incrementing key generator.
  nextKey :: E Word64 <- var "nextKey" $ Just 3

  -- Output variables:

  -- New room key created for checkins.
  roomKey :: E (Word64, Word64) <- var' "newKey" $ Const (0, 0)

  -- Door lock 1 and 2 unlocks.
  unlockDoor1 <- var' "doorUnlock1" $ Const False
  unlockDoor2 <- var' "doorUnlock2" $ Const False

  -- Nondeterminstic input event.
  event :: HotelEvent <- var "event" Nothing
  let key = ProjSnd $ ProjSnd event

  -- Case on the incoming HotelEvent.
  case' event
    -- Checkin to room 1.
    [ (checkinRoom 1, do
        roomKey <== Tuple2 door1 nextKey
        door1   <== nextKey
        nextKey <== Add nextKey (Const 1)
      )

    -- Checkin to room 2.
    , (checkinRoom 2, do
        roomKey <== Tuple2 door2 nextKey
        door2   <== nextKey
        nextKey <== Add nextKey (Const 1)
      )

    -- Checkouts do nothing.  No state is modified.
    , (checkout, return ())

    -- Access attempt room 1.
    , (accessRoom 1, doorLock1 key >>= (unlockDoor1 <==))

    -- Access attempt room 2.
    , (accessRoom 2, doorLock2 key >>= (unlockDoor2 <==))
    ]

  where
  -- Predicates for casing on hotel events.
  checkinRoom :: Word64 -> HotelEvent -> E Bool
  checkinRoom room a = ProjFst a .== Const 1 &&& ProjFst (ProjSnd a) .== Const room
  checkout :: HotelEvent -> E Bool
  checkout a = ProjFst a .== Const 2
  accessRoom :: Word64 -> HotelEvent -> E Bool
  accessRoom room a = ProjFst a .== Const 3 &&& ProjFst (ProjSnd a) .== Const room

