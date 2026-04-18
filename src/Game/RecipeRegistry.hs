module Game.RecipeRegistry
  ( RecipeRegistry
  , newRecipeRegistry
  , registerRecipe
  , registerRecipes
  , findMatchingRecipe
  , allRegisteredRecipes
  , defaultRecipeRegistry
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Game.Crafting (Recipe(..), CraftResult(..), CraftingGrid, allRecipes, extractPattern, matchesPattern)
import Game.Item (Item)

-- | Mutable registry holding all known recipes.
type RecipeRegistry = IORef [Recipe]

-- | Create an empty recipe registry.
newRecipeRegistry :: IO RecipeRegistry
newRecipeRegistry = newIORef []

-- | Register a single recipe.
registerRecipe :: RecipeRegistry -> Recipe -> IO ()
registerRecipe reg recipe = modifyIORef' reg (recipe :)

-- | Register multiple recipes at once.
registerRecipes :: RecipeRegistry -> [Recipe] -> IO ()
registerRecipes reg recipes = modifyIORef' reg (recipes ++)

-- | Return every recipe currently in the registry.
allRegisteredRecipes :: RecipeRegistry -> IO [Recipe]
allRegisteredRecipes = readIORef

-- | Search the registry for a recipe matching the given crafting grid.
findMatchingRecipe :: RecipeRegistry -> CraftingGrid -> IO CraftResult
findMatchingRecipe reg grid = do
  recipes <- readIORef reg
  let pat = extractPattern grid
  pure $ case findFirst pat recipes of
    Just r  -> CraftSuccess (rcResult r) (rcCount r)
    Nothing -> CraftFailure

-- | Create a registry pre-loaded with every recipe from 'allRecipes'.
defaultRecipeRegistry :: IO RecipeRegistry
defaultRecipeRegistry = do
  reg <- newRecipeRegistry
  registerRecipes reg allRecipes
  pure reg

-- | Find the first recipe whose pattern matches.
findFirst :: [[Maybe Item]] -> [Recipe] -> Maybe Recipe
findFirst _ [] = Nothing
findFirst pat (r:rs)
  | matchesPattern (rcPattern r) pat = Just r
  | otherwise = findFirst pat rs
