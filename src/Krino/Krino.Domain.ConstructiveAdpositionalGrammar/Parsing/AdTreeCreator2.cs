using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Diagnostic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeCreator2
    {
        private IConstructiveDictionary myConstructiveDictionary;

        public AdTreeCreator2(IConstructiveDictionary constructiveDictionary)
        {
            myConstructiveDictionary = constructiveDictionary;
        }

        //public List<IAdTree> Create(IEnumerable<string> phrase)
        //{
        //    using var _t = Trace.Entering();

        //    List<IAdTree> results = new List<IAdTree>();

        //    var phraseDecomposition = myConstructiveDictionary.DecomposePhrase(phrase, 0);
        //}



        //private List<WordAdTrees> GetWordAdTrees(PhraseDecomposition phraseDecomposition)
        //{
        //    using (Trace.Entering())
        //    {
        //        // AdTrees for each word construction of each word in the phrase.
        //        List<WordAdTrees> result = new List<WordAdTrees>();


        //        // Go via all words.
        //        foreach (var wordDecomposition in phraseDecomposition.Words)
        //        {
        //            // The adtree for each word construction.
        //            var possibleWordAdTrees = new WordAdTrees(wordDecomposition.Word);

        //            // Go via all possible word constructions (particular combinations of prefixes, lexeme, suffixes).
        //            foreach (var wordComposition in wordDecomposition.Compositions)
        //            {
        //                // List of adtrees for each word construction.
        //                var wordAdTreeDecomposition = new WordAdTreesDecomposition();

        //                // Go via the particular sequence of morphemes (prefixes, lexeme and suffixes) the word is composed of.
        //                foreach (var morpheme in wordComposition.Morphemes)
        //                {
        //                    // Get all possible adtrees for the morpheme.
        //                    var morphemeAdTrees = GetAdTreesForMorpheme(morpheme);

        //                    if (morphemeAdTrees.AdTrees.Any())
        //                    {
        //                        wordAdTreeDecomposition.MorphemeAdTrees.Add(morphemeAdTrees);
        //                    }
        //                }

        //                if (wordAdTreeDecomposition.MorphemeAdTrees.Any())
        //                {
        //                    // Get all variations of morpheme adtrees to compose the word.
        //                    IEnumerable<IEnumerable<IAdTree>> wordAdTreeVariations = wordAdTreeDecomposition.MorphemeAdTrees.Select(x => x.AdTrees).GetVariations();

        //                    foreach (IEnumerable<IAdTree> wordAdTreeVariation in wordAdTreeVariations)
        //                    {
        //                        List<IAdTree> wordAdTrees = ComposeAdTree(wordAdTreeVariation);

        //                        // Try to extend possible addtrees by applying grammar character transferences.
        //                        List<IAdTree> wordAdTreesAndTransferences = wordAdTrees
        //                            .SelectMany(x => new IAdTree[] { x }.Concat(GetGrammarCharacterTransferences(x)))
        //                            .ToList();

        //                        if (wordAdTreesAndTransferences.Any())
        //                        {
        //                            possibleWordAdTrees.AdTrees.AddRange(wordAdTreesAndTransferences);
        //                        }
        //                    }
        //                }
        //            }

        //            if (possibleWordAdTrees.AdTrees.Any())
        //            {
        //                result.Add(possibleWordAdTrees);
        //            }
        //        }

        //        return result;
        //    }
        //}

        private MorphemeRelevantAdTrees GetAdTreesForMorpheme(Morpheme morpheme)
        {
            using (Trace.Entering())
            {
                var result = new MorphemeRelevantAdTrees();

                IEnumerable<Pattern> matchingPatterns = myConstructiveDictionary.FindPatterns(morpheme);

                // Go via patterns matching the morpheme.
                foreach (Pattern pattern in matchingPatterns)
                {
                    // Create the adtree element from the morpheme and its pattern.
                    IAdTree newAdTree = new AdTree(morpheme, pattern);
                    result.AdTrees.Add(newAdTree);
                }

                return result;
            }
        }

        private List<IAdTree> ComposeAdTree(IEnumerable<IAdTree> source)
        {
            using var _t = Trace.Entering();

            List<IAdTree> activeAdTrees = new List<IAdTree>();

            

            return activeAdTrees.Select(x => x.Root).ToList();
        }

    }
}
