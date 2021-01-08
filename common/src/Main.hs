module Domain where

type ConsumerID = String
type MachineID = String
type StoreID = String

type Weight = Double
type Capacity = Double
type Duration = Int

data Promotion
  = PhotoAd Url Duration
  | VideoAd Url

data VRD = VRD MachineID [Promotion] Capacity

data VRDAction 
  = ChangePromotion MachineID Promotion
  | DepositPod ConsumerID MachineID Weight
  | EmptyBin MachineID
  | GreetUser
  deriving Show

data VRD_Error
  = UnableToConnect
  | DoesNotExist
  | NoCapacity

registerVRD :: SerialNumber -> StoreID -> VRD
updateVRD :: (VRDAction a) => a -> Either VRD_Error VRD
deleteVRD :: SerialNumber -> MachineID -> Either VRD_Error SerialNumber
