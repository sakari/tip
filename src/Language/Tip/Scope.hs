{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Language.Tip.Scope (Ref, ScopeMonad(..), ScopeT, runScope ) where
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

newtype Ref = Ref { ref :: Integer }
    deriving (Ord, Eq, Show)

class ScopeMonad m where
    scope :: [String] -> m a -> m a
    resolve :: String -> m (Maybe Ref)

data Env = Env { scopes :: [Scope], freeRef :: Integer }

newtype ScopeT m a = ScopeT { scopeT :: StateT Env m a}
    deriving (Monad, Functor)

type Scope = Map.Map String Ref

instance MonadTrans ScopeT where
    lift = ScopeT . lift

instance (Monad m, Functor m) => ScopeMonad (ScopeT m) where
    scope names block =  ScopeT $ do
                       s <- fmap Map.fromList $ mapM w names
                       modify $ \e -> e { scopes = (s: scopes e) }
                       r <- scopeT block
                       modify $ \e -> e { scopes = tail $ scopes e }
                       return r
        where
          w name = do
            r <- gets freeRef
            modify $ \e -> e { freeRef = r + 1 }
            return (name, Ref r)
    resolve k = ScopeT $ gets w
        where
          w Env { scopes } = case mapMaybe (Map.lookup k) scopes of
                               [] -> Nothing
                               (s:_) -> Just s


runScope :: Monad m => ScopeT m a -> m a
runScope s = scopeT s `evalStateT` Env {scopes = [], freeRef = 0}
