
import Test.Hspec

import Lib
import Defaults
import Types

main :: IO ()
main = hspec $ do
  describe "default font" $ do
    it "renders space correctly" $ do
      render " " defaultFont `shouldBe` concat [
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            \n",
        "            "
        ]

    it "renders single letter correctly" $ do
      render "A" defaultFont `shouldBe` concat [
        "            \n",
        "            \n",
        "     @@@    \n",
        "    @   @   \n",
        "    @   @   \n",
        "    @   @   \n",
        "    @@@@@   \n",
        "    @   @   \n",
        "    @   @   \n",
        "    @   @   \n",
        "            \n",
        "            "
        ]

    it "renders a few letters correctly" $ do
      render "Ab" defaultFont `shouldBe` concat [
        "                        \n",
        "                        \n",
        "     @@@        @       \n",
        "    @   @       @       \n",
        "    @   @       @@@@    \n",
        "    @   @       @   @   \n",
        "    @@@@@       @   @   \n",
        "    @   @       @   @   \n",
        "    @   @       @   @   \n",
        "    @   @       @@@@    \n",
        "                        \n",
        "                        "
        ]

    it "has ability to change a filler" $ do
      render "Ab" (defaultFont {filler='*'}) `shouldBe` concat [
        "                        \n",
        "                        \n",
        "     ***        *       \n",
        "    *   *       *       \n",
        "    *   *       ****    \n",
        "    *   *       *   *   \n",
        "    *****       *   *   \n",
        "    *   *       *   *   \n",
        "    *   *       *   *   \n",
        "    *   *       ****    \n",
        "                        \n",
        "                        "
        ]