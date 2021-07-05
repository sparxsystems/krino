using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Rules;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    /// <summary>
    /// Functionality to create AdTree.
    /// </summary>
    public class AdTreeCreator
    {
        private IConstructiveDictionary myConstructiveDictionary;

        public AdTreeCreator(IConstructiveDictionary constructiveDictionary)
        {
            myConstructiveDictionary = constructiveDictionary;
        }

        /// <summary>
        /// Creates AdTree from the provided phrase.
        /// </summary>
        /// <param name="phrase"></param>
        /// <returns></returns>
        public List<IAdTree> Create(params string[] phrase)
        {
            using (Trace.Entering())
            {
                return Create((IEnumerable<string>)phrase);
            }
        }

        /// <summary>
        /// Creates AdTree from the provided phrase.
        /// </summary>
        /// <param name="phrase"></param>
        /// <returns></returns>
        public List<IAdTree> Create(IEnumerable<string> phrase)
        {
            using (Trace.Entering())
            {
                List<IAdTree> results = new List<IAdTree>();

                var phraseDecomposition = myConstructiveDictionary.DecomposePhrase(phrase, 0);

                List<WordAdTrees> wordsAdTrees = GetWordAdTrees(phraseDecomposition);
                List<List<IAdTree>> wordsVariations = wordsAdTrees.Select(x => x.AdTrees).GetVariations().Select(x => x.ToList()).ToList();

                foreach (List<IAdTree> wordsVariation in wordsVariations)
                {
                    // If it is only one word.
                    if (wordsVariation.Count == 1)
                    {
                        results.Add(wordsVariation[0]);
                    }
                    // Multiple words or a sentence.
                    else
                    {
                        var patternSignature = string.Join("", wordsVariation.Select(x => x.Pattern.UpRule.GrammarCharacter));

                        var adTreeFactories = myConstructiveDictionary.FindAdTreeConstructions(patternSignature);
                        if (adTreeFactories != null)
                        {
                            foreach (var factory in adTreeFactories)
                            {
                                var adTree = factory.CreateAdTree(myConstructiveDictionary.AttributesModel, wordsVariation);
                                results.Add(adTree);
                            }
                        }
                    }
                }

                return results;
            }
        }


        private List<WordAdTrees> GetWordAdTrees(PhraseDecomposition phraseDecomposition)
        {
            using (Trace.Entering())
            {
                // AdTrees for each word construction of each word in the phrase.
                List<WordAdTrees> result = new List<WordAdTrees>();


                // Go via all words.
                foreach (var wordDecomposition in phraseDecomposition.Words)
                {
                    // The adtree for each word construction.
                    var possibleWordAdTrees = new WordAdTrees(wordDecomposition.Word);

                    // Go via all possible word constructions (particular combinations of prefixes, lexeme, suffixes).
                    foreach (var wordComposition in wordDecomposition.Compositions)
                    {
                        // List of adtrees for each word construction.
                        var wordAdTreeDecomposition = new WordAdTreesDecomposition();

                        // Go via the particular sequence of morphemes (prefixes, lexeme and suffixes) the word is composed of.
                        foreach (var morpheme in wordComposition.Morphemes)
                        {
                            // Get all possible adtrees for the morpheme.
                            var morphemeAdTrees = GetAdTreesForMorpheme(morpheme);

                            if (morphemeAdTrees.AdTrees.Any())
                            {
                                wordAdTreeDecomposition.MorphemeAdTrees.Add(morphemeAdTrees);
                            }
                        }

                        if (wordAdTreeDecomposition.MorphemeAdTrees.Any())
                        {
                            // Get all variations of morpheme adtrees to compose the word.
                            IEnumerable<IEnumerable<IAdTree>> wordAdTreeVariations = wordAdTreeDecomposition.MorphemeAdTrees.Select(x => x.AdTrees).GetVariations();

                            foreach (IEnumerable<IAdTree> wordAdTreeVariation in wordAdTreeVariations)
                            {
                                List<IAdTree> wordAdTrees = ComposeAdTree(wordAdTreeVariation);

                                // Try to extend possible addtrees by applying grammar character transferences.
                                List<IAdTree> wordAdTreesAndTransferences = wordAdTrees
                                    .SelectMany(x => new IAdTree[] { x }.Concat(GetGrammarCharacterTransferences(x)))
                                    .ToList();

                                if (wordAdTreesAndTransferences.Any())
                                {
                                    possibleWordAdTrees.AdTrees.AddRange(wordAdTreesAndTransferences);
                                }
                            }
                        }
                    }

                    if (possibleWordAdTrees.AdTrees.Any())
                    {
                        result.Add(possibleWordAdTrees);
                    }
                }

                return result;
            }
        }

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

        private IEnumerable<IAdTree> GetGrammarCharacterTransferences(IAdTree adTree)
        {
            using (Trace.Entering())
            {
                IEnumerable<Pattern> patterns = myConstructiveDictionary.FindMonoTransferencePatterns(adTree.Morpheme);
                foreach (Pattern pattern in patterns)
                {
                    // Note: in case of a grammar character transference it is expected the morpheme rule always represents
                    //       a concrete value.
                    if (pattern.UpRule.AttributesRule is IValueRule<BigInteger> isRule)
                    {
                        IAdTree adTreeCopy = adTree.MakeShallowCopy();
                        IAdTree transferenceAdTree = new AdTree(new Morpheme(myConstructiveDictionary.AttributesModel, "", isRule.Value), pattern);
                        transferenceAdTree.Right = adTreeCopy;
                        yield return transferenceAdTree;
                    }
                }
            }
        }

        private List<IAdTree> ComposeAdTree(IEnumerable<IAdTree> source)
        {
            using var _t = Trace.Entering();

            var relevantPatterns = myConstructiveDictionary.Patterns.Where(x => x.IsPairTransference).ToList();

            List<IAdTree> activeAdTrees = new List<IAdTree>();

            bool isFirst = true;
            foreach (IAdTree adTree in source)
            {
                if (isFirst)
                {
                    activeAdTrees.Add(adTree);
                    isFirst = false;
                }
                else
                {
                    List<IAdTree> newListOfAdTrees = new List<IAdTree>();
                    foreach (IAdTree current in activeAdTrees)
                    {
                        var suitableParentPatterns = GetSuitablePrentPatterns(relevantPatterns, current.Pattern, adTree.Pattern);
                        foreach (var parentPattern in suitableParentPatterns)
                        {
                            var parentAdTree = new AdTree(Morpheme.Epsilon(myConstructiveDictionary.AttributesModel), parentPattern);
                            if (parentPattern.IsLeftFirst)
                            {
                                parentAdTree.Left = current.MakeShallowCopy();
                                parentAdTree.Right = adTree.MakeShallowCopy();
                            }
                            else
                            {
                                parentAdTree.Right = current.MakeShallowCopy();
                                parentAdTree.Left = adTree.MakeShallowCopy();
                            }

                            newListOfAdTrees.Add(parentAdTree);
                        }
                    }

                    activeAdTrees = newListOfAdTrees;
                    if (activeAdTrees.Count == 0)
                    {
                        break;
                    }
                }
            }

            return activeAdTrees.Select(x => x.Root).ToList();
        }

        private IEnumerable<Pattern> GetSuitablePrentPatterns(IEnumerable<Pattern> patterns, Pattern firstChild, Pattern secondChild)
        {
            foreach (var pattern in patterns)
            {
                if (pattern.IsLeftFirst)
                {
                    if (pattern.LeftPatternRule.Evaluate(firstChild))
                    {
                        if (pattern.RightPatternRule.Evaluate(secondChild))
                        {
                            yield return pattern;
                        }
                    }
                }
                else
                {
                    if (pattern.RightPatternRule.Evaluate(firstChild))
                    {
                        if (pattern.LeftPatternRule.Evaluate(secondChild))
                        {
                            yield return pattern;
                        }
                    }
                }
            }
        }
    }
}
