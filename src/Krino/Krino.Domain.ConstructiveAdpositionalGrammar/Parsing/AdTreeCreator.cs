﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
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
        private IAttributesModel myAttributesModel;

        public AdTreeCreator(IAttributesModel attributesModel, IConstructiveDictionary constructiveDictionary)
        {
            myConstructiveDictionary = constructiveDictionary;
            myAttributesModel = attributesModel;
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
                    List<IAdTree> phraseAdTree = ComposeAdTree(wordsVariation);

                    // Add only adtrees without errors.
                    results.AddRange(phraseAdTree.Where(x => !x.GetNonconformities(myAttributesModel).Any()));
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
                IEnumerable<Pattern> patterns = myConstructiveDictionary.FindGrammarCharacterTransferencePatterns(adTree.Morpheme);
                foreach (Pattern pattern in patterns)
                {
                    // Note: in case of a grammar character transference it is expected the morpheme rule always represents
                    //       a concrete value.
                    if (pattern.MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> isRule)
                    {
                        IAdTree adTreeCopy = adTree.MakeShallowCopy();
                        IAdTree transferenceAdTree = new AdTree(new Morpheme(myAttributesModel, "", isRule.ReferenceValue), pattern);
                        transferenceAdTree.Right = adTreeCopy;
                        yield return transferenceAdTree;
                    }
                }
            }
        }

        private List<IAdTree> ComposeAdTree(IEnumerable<IAdTree> source)
        {
            using (Trace.Entering())
            {
                List<IAdTree> activeAdTrees = new List<IAdTree>();

                string expectedSignature = source.GetSignature();

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
                            TryToAppend(current, adTree, expectedSignature, newListOfAdTrees);
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
        }

        private void TryToAppend(IAdTree current, IAdTree appendee, string expectedSignature, List<IAdTree> results)
        {
            using (Trace.Entering())
            {
                IAdTree placeToAppend = GetPlaceToAppend(current);

                // If an adposition needs to be filled.
                if (!placeToAppend.Pattern.MorphemeRule.Equals(MorphemeRule.Nothing) &&
                    !placeToAppend.Pattern.MorphemeRule.Evaluate(placeToAppend.Morpheme))
                {
                    if (placeToAppend.Pattern.Equals(appendee.Pattern))
                    {
                        IAdTree newElementCopy = appendee.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        adTreeCopy.Replace(newElementCopy);
                        results.Add(newElementCopy);
                    }

                    return;
                }

                bool isMorphematicAdPosition = appendee.Pattern.IsMorphematicAdPosition();
                bool canAppendIndirectly = CanAppendIndirectly(current) && !isMorphematicAdPosition;


                // If the new element can be attached to left.
                if (placeToAppend.Left == null && !placeToAppend.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
                {
                    // Try to attach the new element directly.
                    if (placeToAppend.CanAttachToLeft(appendee, myAttributesModel))
                    {
                        IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        adTreeCopy.Left = newAdTreeElementCopy;
                        results.Add(newAdTreeElementCopy);
                    }
                    if (canAppendIndirectly)
                    {
                        // Try to attach the new element indirectly.
                        TryToAppendIndirectly(AttachingPosition.ParrentForLeft, placeToAppend, appendee, expectedSignature, results);
                    }
                }

                // If the new element can be attached to right.
                if (placeToAppend.Right == null && !placeToAppend.Pattern.RightRule.Equals(MorphemeRule.Nothing))
                {
                    // Try to attach the new element directly.
                    if (placeToAppend.CanAttachToRight(appendee, myAttributesModel))
                    {
                        IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        adTreeCopy.Right = newAdTreeElementCopy;
                        results.Add(newAdTreeElementCopy);
                    }
                    if (canAppendIndirectly)
                    {
                        // Try to attach the new element indirectly.
                        TryToAppendIndirectly(AttachingPosition.ParrentForRight, placeToAppend, appendee, expectedSignature, results);
                    }
                }

                // If the new element can attach the placeToAppend to its left or right.
                // If the adtree has free adposition - i.e. it can be attached to left or right.
                if (placeToAppend.AdPosition == null)
                {
                    // The new element tries to attach the adtree to its left.
                    if (appendee.Left == null && !appendee.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
                    {
                        if (appendee.CanAttachToLeft(placeToAppend, myAttributesModel))
                        {
                            IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                            IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                            newAdTreeElementCopy.Left = adTreeCopy;
                            results.Add(adTreeCopy);
                        }
                        // Try to attach the adtree indirectly via new element's left.
                        else if (canAppendIndirectly)
                        {
                            TryToAppendIndirectly(AttachingPosition.ParrentForLeft, appendee, placeToAppend, expectedSignature, results);
                        }
                    }

                    // The new element tries to attach the adtree to its right.
                    if (appendee.Right == null && !appendee.Pattern.RightRule.Equals(MorphemeRule.Nothing))
                    {
                        if (appendee.CanAttachToRight(placeToAppend, myAttributesModel))
                        {
                            IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                            IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                            newAdTreeElementCopy.Right = adTreeCopy;
                            results.Add(adTreeCopy);
                        }
                        // Try to attach the adtree indirectly via new element's right.
                        else if (canAppendIndirectly)
                        {
                            TryToAppendIndirectly(AttachingPosition.ParrentForRight, appendee, placeToAppend, expectedSignature, results);
                        }
                    }

                    if (canAppendIndirectly)
                    {
                        // Also try to connect the adtree and the new element indirectly via adtree adposition.
                        TryToAppendIndirectly(AttachingPosition.ChildOnLeft, placeToAppend, appendee, expectedSignature, results);
                        TryToAppendIndirectly(AttachingPosition.ChildOnRight, placeToAppend, appendee, expectedSignature, results);
                    }
                }
            }
        }

        // Note: indirect append means that adtress (with unfilled values) will be inserted inbetween start and end adtree.
        //       Unfilled values are expected to be filled later otherwise the adtree will be not valid.
        private void TryToAppendIndirectly(AttachingPosition appendingPositionOfStartElement,
            IAdTree start, IAdTree end,
            string expectedSignature,
            List<IAdTree> results)
        {
            using (Trace.Entering())
            {
                GrammarCharacter startGrammarCharacter = GrammarCharacter.e;
                GrammarCharacter endGrammarCharacter = GrammarCharacter.e;

                // If start adTree connects the bridge on its left.
                if (appendingPositionOfStartElement == AttachingPosition.ParrentForLeft)
                {
                    startGrammarCharacter = start.Pattern.LeftRule.GrammarCharacter;
                    endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.RulingGrammarCharacter);
                }
                // If start adTree connects the bridge on its right.
                else if (appendingPositionOfStartElement == AttachingPosition.ParrentForRight)
                {
                    startGrammarCharacter = start.Pattern.RightRule.GrammarCharacter;
                    endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.RulingGrammarCharacter);
                }
                // If the bridge connects the start adtree on its left.
                else if (appendingPositionOfStartElement == AttachingPosition.ChildOnLeft)
                {
                    startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.GrammarCharacter, start.RulingGrammarCharacter);
                    endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.RulingGrammarCharacter);
                }
                // If the bridge connects the start adtree on its right.
                else if (appendingPositionOfStartElement == AttachingPosition.ChildOnRight)
                {
                    startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.GrammarCharacter, start.RulingGrammarCharacter);
                    endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.RulingGrammarCharacter);
                }

                // Get all possibile ways how to get from start to end grammar character and path is not greater than 4.
                IEnumerable<IReadOnlyList<DirectedEdge<GrammarCharacter, Pattern>>> connectionPaths = myConstructiveDictionary.PatternGraph
                    .FindAllEdges(startGrammarCharacter, endGrammarCharacter).ToList();

                // Go via all possible ways.
                foreach (IReadOnlyList<DirectedEdge<GrammarCharacter, Pattern>> path in connectionPaths)
                {
                    IAdTree startCopy = start.MakeShallowCopy();
                    IAdTree endCopy = end.MakeShallowCopy();

                    IAdTree previousBridge = null;

                    // Go via the path.
                    for (int i = 0; i < path.Count; ++i)
                    {
                        DirectedEdge<GrammarCharacter, Pattern> edge = path[i];

                        BigInteger attributes = edge.Value.MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> referenceAttributes ? referenceAttributes.ReferenceValue : myAttributesModel.GetAttributes(edge.Value.MorphemeRule.GrammarCharacter);
                        IAdTree bridge = new AdTree(new Morpheme(myAttributesModel, "", attributes), edge.Value);

                        // If it is the first item on the path.
                        if (i == 0)
                        {
                            if (appendingPositionOfStartElement == AttachingPosition.ParrentForLeft)
                            {
                                if (startCopy.Left == null && startCopy.CanAttachToLeft(bridge, myAttributesModel))
                                {
                                    startCopy.Left = bridge;
                                }
                                else
                                {
                                    break;
                                }
                            }
                            else if (appendingPositionOfStartElement == AttachingPosition.ParrentForRight)
                            {
                                if (startCopy.Right == null && startCopy.CanAttachToRight(bridge, myAttributesModel))
                                {
                                    startCopy.Right = bridge;
                                }
                                else
                                {
                                    break;
                                }
                            }
                            else if (appendingPositionOfStartElement == AttachingPosition.ChildOnLeft)
                            {
                                if (bridge.Left == null && bridge.CanAttachToLeft(startCopy, myAttributesModel))
                                {
                                    bridge.Left = startCopy;
                                }
                                else
                                {
                                    break;
                                }
                            }
                            else if (appendingPositionOfStartElement == AttachingPosition.ChildOnRight)
                            {
                                if (bridge.Right == null && bridge.CanAttachToRight(startCopy, myAttributesModel))
                                {
                                    bridge.Right = startCopy;
                                }
                                else
                                {
                                    break;
                                }
                            }
                        }
                        else
                        {
                            if (previousBridge.Left == null && previousBridge.CanAttachToLeft(bridge, myAttributesModel))
                            {
                                previousBridge.Left = bridge;
                            }
                            else if (previousBridge.Right == null && previousBridge.CanAttachToRight(bridge, myAttributesModel))
                            {
                                previousBridge.Right = bridge;
                            }
                            else
                            {
                                break;
                            }
                        }

                        // If the signature does not match then break it.
                        var currentSignature = bridge.Root.GetSignature();
                        if (!expectedSignature.StartsWith(currentSignature))
                        {
                            break;
                        }

                        // If it is the last item in the path.
                        if (i == path.Count - 1)
                        {
                            if (bridge.Left == null && bridge.CanAttachToLeft(endCopy, myAttributesModel))
                            {
                                bridge.Left = endCopy;

                                currentSignature = endCopy.Root.GetSignature();
                                if (expectedSignature.StartsWith(currentSignature))
                                {
                                    results.Add(endCopy);
                                }
                            }
                            else if (bridge.Right == null && bridge.CanAttachToRight(endCopy, myAttributesModel))
                            {
                                bridge.Right = endCopy;

                                currentSignature = endCopy.Root.GetSignature();
                                if (expectedSignature.StartsWith(currentSignature))
                                {
                                    results.Add(endCopy);
                                }
                            }
                            else
                            {
                                break;
                            }
                        }

                        previousBridge = bridge;
                    }
                }
            }
        }

        private bool CanAppendIndirectly(IAdTree current)
        {
            using (Trace.Entering())
            {
                IEnumerable<IAdTree> incompleteAdTrees = current.GetSequenceToRoot().Where(x => !x.IsComplete()).Take(5);
                int count = incompleteAdTrees.Count();
                return count < 5;
            }
        }

        private IAdTree GetPlaceToAppend(IAdTree current)
        {
            using (Trace.Entering())
            {
                IEnumerable<IAdTree> adTrees = current.GetSequenceToRoot();
                foreach (IAdTree adTree in adTrees)
                {
                    // If left or right position can be filled.
                    if (adTree.Left == null && !adTree.Pattern.LeftRule.Equals(MorphemeRule.Nothing) ||
                        adTree.Right == null && !adTree.Pattern.RightRule.Equals(MorphemeRule.Nothing) ||
                        // or an adposition morpheme is still expected.
                        !adTree.Pattern.MorphemeRule.Equals(MorphemeRule.Nothing) &&
                        !adTree.Pattern.MorphemeRule.Evaluate(adTree.Morpheme))
                    {
                        return adTree;
                    }
                }

                return current.Root;
            }
        }

        private GrammarCharacter ChooseGrammarCharacter(params GrammarCharacter[] grammarCharacters)
        {
            using (Trace.Entering())
            {
                GrammarCharacter result = GrammarCharacter.e;

                foreach (GrammarCharacter grammarCharacter in grammarCharacters)
                {
                    if (grammarCharacter != GrammarCharacter.e)
                    {
                        result = grammarCharacter;

                        if (grammarCharacter != GrammarCharacter.U)
                        {
                            break;
                        }
                    }
                }

                return result;
            }
        }
    }
}
