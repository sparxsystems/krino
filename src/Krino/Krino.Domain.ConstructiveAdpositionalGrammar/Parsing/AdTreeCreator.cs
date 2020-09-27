using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeCreator
    {
        private IConstructiveDictionary myConstructiveDictionary;
        private IReadOnlyList<IReadOnlyList<IReadOnlyList<Morpheme>>> myDecomposedPhrase;

        public AdTreeCreator(IConstructiveDictionary constructiveDictionary, IEnumerable<string> words)
        {
            myConstructiveDictionary = constructiveDictionary;
            myDecomposedPhrase = myConstructiveDictionary.DecomposePhrase(words, 0);
        }

        public IAdTree Create()
        {
            List<List<IAdTree>> wordsAdTrees = GetAllAdTreesForAllWords();
            List<List<IAdTree>> wordVariations = wordsAdTrees.GetVariations().Select(x => x.ToList()).ToList();

            foreach (List<IAdTree> variation in wordVariations)
            {
                List<IAdTree> results = ComposeAdTree(variation);
            }

            return null;
        }



        private IEnumerable<IAdTree> GetAdTreesForMorpheme(Morpheme morpheme)
        {
            IEnumerable<Pattern> matchingPatterns = myConstructiveDictionary.FindPatterns(morpheme);

            // Go via patterns matching the morpheme.
            foreach (Pattern pattern in matchingPatterns)
            {
                // Create the adtree element from the morpheme and its pattern.
                IAdTree newAdTree = new AdTree(morpheme, pattern);
                yield return newAdTree;
            }
        }


        private IEnumerable<IAdTree> GetGrammarCharacterTransferences(IAdTree adTree)
        {
            IEnumerable<Pattern> patterns = myConstructiveDictionary.FindGrammarCharacterTransferencePatterns(adTree.Morpheme);
            foreach (Pattern pattern in patterns)
            {
                // Note: in case of a grammar character transference it is expected the morpheme rule always represents
                //       a concrete value.
                if (pattern.MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> isRule)
                {
                    IAdTree adTreeCopy = adTree.MakeShallowCopy();
                    IAdTree transferenceAdTree = new AdTree(new Morpheme("", isRule.ReferenceValue), pattern);
                    transferenceAdTree.Right = adTreeCopy;
                    yield return transferenceAdTree;
                }
            }
        }

        private List<List<IAdTree>> GetAllAdTreesForAllWords()
        {
            // wordIdx, wordConstructionIdx, 
            List<List<IAdTree>> result = new List<List<IAdTree>>();


            // Go via all words.
            foreach (IReadOnlyList<IReadOnlyList<Morpheme>> possibleWordConstructions in myDecomposedPhrase)
            {
                List<IAdTree> particularWordAdTrees = new List<IAdTree>();

                // Go via all possible word constructions (all combinations of prefixes, lexeme, suffixes).
                foreach (IReadOnlyList<Morpheme> wordConstruction in possibleWordConstructions)
                {
                    List<List<IAdTree>> particularWordConstruction = new List<List<IAdTree>>();

                    // Go via the particular sequence of prefixes, lexeme, suffixes the word is composed of.
                    foreach (Morpheme morphemeItem in wordConstruction)
                    {
                        // Get all possible adtrees for the morpheme item.
                        IEnumerable<IAdTree> morphemeAdTrees = GetAdTreesForMorpheme(morphemeItem);

                        // Try to extend possible addtrees by applying grammar character transferences.
                        //List<IAdTree> morphemeAndTransferences = morphemeAdTrees
                        //    .SelectMany(x => new IAdTree[] { x }.Concat(GetGrammarCharacterTransferences(x)))
                        //    .ToList();

                        if (morphemeAdTrees.Any())
                        {
                            particularWordConstruction.Add(morphemeAdTrees.ToList());
                        }
                    }

                    if (particularWordConstruction.Any())
                    {
                        IEnumerable<IEnumerable<IAdTree>> wordConstructionVariations = particularWordConstruction.GetVariations();


                        foreach (IEnumerable<IAdTree> wordVariation in wordConstructionVariations)
                        {
                            List<IAdTree> wordAdtrees = ComposeAdTree(wordVariation);

                            // Try to extend possible addtrees by applying grammar character transferences.
                            List<IAdTree> wordAdTreesAndTransferences = wordAdtrees
                                .SelectMany(x => new IAdTree[] { x }.Concat(GetGrammarCharacterTransferences(x)))
                                .ToList();

                            particularWordAdTrees.AddRange(wordAdTreesAndTransferences);
                        }
                    }
                }

                result.Add(particularWordAdTrees);
            }

            return result;
        }


        private List<IAdTree> ComposeAdTree(IEnumerable<IAdTree> source)
        {
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
                        TryToAppend(current, adTree, newListOfAdTrees);
                    }

                    activeAdTrees = newListOfAdTrees;
                    if (activeAdTrees.Count == 0)
                    {
                        break;
                    }
                }
            }

            return activeAdTrees;
        }

        private void TryToAppend(IAdTree current, IAdTree appendee, List<IAdTree> results)
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
                if (placeToAppend.CanAttachToLeft(appendee))
                {
                    IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                    IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                    adTreeCopy.Left = newAdTreeElementCopy;
                    results.Add(newAdTreeElementCopy);
                }
                if (canAppendIndirectly)
                {
                    // Try to attach the new element indirectly.
                    TryToAppendIndirectly(AttachPosition.ParrentForLeft, placeToAppend, appendee, results, 2);
                }
            }

            // If the new element can be attached to right.
            if (placeToAppend.Right == null && !placeToAppend.Pattern.RightRule.Equals(MorphemeRule.Nothing))
            {
                // Try to attach the new element directly.
                if (placeToAppend.CanAttachToRight(appendee))
                {
                    IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                    IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                    adTreeCopy.Right = newAdTreeElementCopy;
                    results.Add(newAdTreeElementCopy);
                }
                if (canAppendIndirectly)
                {
                    // Try to attach the new element indirectly.
                    TryToAppendIndirectly(AttachPosition.ParrentForRight, placeToAppend, appendee, results, 2);
                }
            }

            // If the new element can attach the placeToAppend to its left or right.
            // If the adtree has free adposition - i.e. it can be attached to left or right.
            if (placeToAppend.AdPosition == null)
            {
                // The new element tries to attach the adtree to its left.
                if (appendee.Left == null && !appendee.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
                {
                    if (appendee.CanAttachToLeft(placeToAppend))
                    {
                        IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        newAdTreeElementCopy.Left = adTreeCopy;
                        results.Add(adTreeCopy);
                    }
                    // Try to attach the adtree indirectly via new element's left.
                    else if (canAppendIndirectly)
                    {
                        TryToAppendIndirectly(AttachPosition.ParrentForLeft, appendee, placeToAppend, results, 2);
                    }
                }

                // The new element tries to attach the adtree to its right.
                if (appendee.Right == null && !appendee.Pattern.RightRule.Equals(MorphemeRule.Nothing))
                {
                    if (appendee.CanAttachToRight(placeToAppend))
                    {
                        IAdTree newAdTreeElementCopy = appendee.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        newAdTreeElementCopy.Right = adTreeCopy;
                        results.Add(adTreeCopy);
                    }
                    // Try to attach the adtree indirectly via new element's right.
                    else if (canAppendIndirectly)
                    {
                        TryToAppendIndirectly(AttachPosition.ParrentForRight, appendee, placeToAppend, results, 2);
                    }
                }

                if (canAppendIndirectly)
                {
                    // Also try to connect the adtree and the new element indirectly via adtree adposition.
                    TryToAppendIndirectly(AttachPosition.ChildOnLeft, placeToAppend, appendee, results, 2);
                    TryToAppendIndirectly(AttachPosition.ChildOnRight, placeToAppend, appendee, results, 2);
                }
            }
        }

        private void TryToAppendIndirectly(AttachPosition appendingPositionOfStartElement, IAdTree start, IAdTree end, List<IAdTree> results, int stopLevel)
        {
            if (stopLevel <= 0)
            {
                return;
            }
            --stopLevel;



            GrammarCharacter startGrammarCharacter = GrammarCharacter.e;
            GrammarCharacter endGrammarCharacter = GrammarCharacter.e;

            // If start adTree connects the bridge via its left.
            if (appendingPositionOfStartElement == AttachPosition.ParrentForLeft)
            {
                startGrammarCharacter = start.Pattern.LeftRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If start adTree connects the bridge via its right.
            else if (appendingPositionOfStartElement == AttachPosition.ParrentForRight)
            {
                startGrammarCharacter = start.Pattern.RightRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its left.
            else if (appendingPositionOfStartElement == AttachPosition.ChildOnLeft)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its right.
            else if (appendingPositionOfStartElement == AttachPosition.ChildOnRight)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }

            // Get possibile ways how to connect new element.
            IEnumerable<IReadOnlyList<DirectedEdge<GrammarCharacter, Pattern>>> connectionPaths = myConstructiveDictionary.PatternGraph
                .FindAllEdges(startGrammarCharacter, endGrammarCharacter, x => x.Count < 4);

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

                    BigInteger attributes = edge.Value.MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> referenceAttributes ? referenceAttributes.ReferenceValue : edge.Value.MorphemeRule.GrammarCharacter.GetAttributes();
                    IAdTree bridge = new AdTree(new Morpheme("", attributes), edge.Value);

                    // If it is the first item on the path.
                    if (i == 0)
                    {
                        if (appendingPositionOfStartElement == AttachPosition.ParrentForLeft)
                        {
                            if (startCopy.Left == null && startCopy.CanAttachToLeft(bridge))
                            {
                                startCopy.Left = bridge;
                            }
                            else
                            {
                                break;
                            }
                        }
                        else if (appendingPositionOfStartElement == AttachPosition.ParrentForRight)
                        {
                            if (startCopy.Right == null && startCopy.CanAttachToRight(bridge))
                            {
                                startCopy.Right = bridge;
                            }
                            else
                            {
                                break;
                            }
                        }
                        else if (appendingPositionOfStartElement == AttachPosition.ChildOnLeft)
                        {
                            if (bridge.Left == null && bridge.CanAttachToLeft(startCopy))
                            {
                                bridge.Left = startCopy;
                            }
                            else
                            {
                                break;
                            }
                        }
                        else if (appendingPositionOfStartElement == AttachPosition.ChildOnRight)
                        {
                            if (bridge.Right == null && bridge.CanAttachToRight(startCopy))
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
                        if (previousBridge.Left == null && previousBridge.CanAttachToLeft(bridge))
                        {
                            previousBridge.Left = bridge;
                        }
                        else if (previousBridge.Right == null && previousBridge.CanAttachToRight(bridge))
                        {
                            previousBridge.Right = bridge;
                        }
                        else
                        {
                            break;
                        }
                    }

                    // If it is the last item in the path.
                    if (i == path.Count - 1)
                    {
                        if (bridge.Left == null && bridge.CanAttachToLeft(end))
                        {
                            TryToAppendIndirectly(AttachPosition.ParrentForLeft, bridge, endCopy, results, stopLevel);

                            bridge.Left = endCopy;
                            results.Add(endCopy);
                        }
                        else if (bridge.Right == null && bridge.CanAttachToRight(end))
                        {
                            TryToAppendIndirectly(AttachPosition.ParrentForRight, bridge, endCopy, results, stopLevel);

                            bridge.Right = endCopy;
                            results.Add(endCopy);
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

        private bool CanAppendIndirectly(IAdTree current)
        {
            IEnumerable<IAdTree> incomleteAdTrees = current.GetSequenceToRoot().Where(x => !x.IsComplete()).Take(5);
            int count = incomleteAdTrees.Count();
            return count < 5;
        }

        private IAdTree GetPlaceToAppend(IAdTree current)
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

        private GrammarCharacter ChooseGrammarCharacter(params GrammarCharacter[] grammarCharacters)
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
