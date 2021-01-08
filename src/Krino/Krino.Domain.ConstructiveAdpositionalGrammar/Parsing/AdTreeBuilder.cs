using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeBuilder
    {
        private IConstructiveDictionary myConstructiveDictionary;

        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        public AdTreeBuilder(IConstructiveDictionary constructiveDictionary)
        {
            myConstructiveDictionary = constructiveDictionary;
        }

        public IReadOnlyList<IAdTree> ActiveAdTrees => myActiveAdTrees.Select(x => x.Root).ToList();

        /// <summary>
        /// Keeps only adtrees without nonconformities.
        /// </summary>
        public void Purify()
        {
            List<IAdTree> adTrees = new List<IAdTree>();

            foreach (IAdTree adTree in myActiveAdTrees)
            {
                IEnumerable<IAdTree> nonconformities = adTree.Root.GetNonconformities();
                int nonConformities = nonconformities.Count();
                if (nonConformities == 0)
                {
                    adTrees.Add(adTree);
                }
            }

            myActiveAdTrees = adTrees;
        }

        public void AddPhrase(IEnumerable<string> words)
        {
            var decomposedPhrase = myConstructiveDictionary.DecomposePhrase(words, 0);
        }

        /// <summary>
        /// Appends a word.
        /// </summary>
        /// <remarks>
        /// If the word consists of several morphemes then they are recognized and the word is decomposed to the adtree.
        /// </remarks>
        /// <param name="word"></param>
        /// <param name="maxMorphDistance">max accepted distance by the Levenshtein algorithm.</param>
        /// <returns></returns>
        public bool AddWord(string word, int maxMorphDistance = 0)
        {
            List<IAdTree> adTreesToAdd = new List<IAdTree>();

            // Decompose the word string to list of morpheme sequences (prefixes, lexeme, suffixes).
            var morphemeSequences = myConstructiveDictionary.DecomposeWord(word, 0);

            if (!morphemeSequences.Compositions.Any())
            {
                // The typo tolerance does not have a sense for very short words.
                int morphDistance = word.Length < 4 ? 0 : maxMorphDistance;

                // Decompose the word string to list of morpheme sequences (prefixes, lexeme, suffixes).
                morphemeSequences = myConstructiveDictionary.DecomposeWord(word, morphDistance);
            }

            // Go via sequences of morphemes.
            foreach (var sequence in morphemeSequences.Compositions)
            {
                bool isCancelled = false;
                AdTreeBuilder localAdTreeBuilder = new AdTreeBuilder(myConstructiveDictionary);
                foreach (var morpheme in sequence.Morphemes)
                {
                    if (!localAdTreeBuilder.AddMorpheme(morpheme))
                    {
                        isCancelled = true;
                        break;
                    }
                }

                if (!isCancelled)
                {
                    adTreesToAdd.AddRange(localAdTreeBuilder.ActiveAdTrees);
                }
            }

            IEnumerable<Morpheme> nonLexemes = myConstructiveDictionary.FindNonLexemes(word);
            IEnumerable<IAdTree> adTrees = GetAdTrees(nonLexemes);
            adTreesToAdd.AddRange(adTrees);

            bool result = AddHomonyms(adTreesToAdd);
            return result;
        }

        /// <summary>
        /// Appends the morpheme.
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        public bool AddMorpheme(Morpheme morpheme)
        {
            bool result = AddHomonyms(new Morpheme[] { morpheme });
            return result;
        }

        /// <summary>
        /// Appends the list of homonym morphemes.
        /// </summary>
        /// <param name="homonymMorphemes"></param>
        /// <returns></returns>
        public bool AddHomonyms(IEnumerable<Morpheme> homonymMorphemes)
        {
            IEnumerable<IAdTree> homonymAdTrees = GetAdTrees(homonymMorphemes);
            bool isAdded = AddHomonyms(homonymAdTrees);
            return isAdded;
        }

        /// <summary>
        /// Appends the list of homonym morphemes stored in adtrees.
        /// </summary>
        /// <param name="homonymAdTrees"></param>
        /// <returns></returns>
        public bool AddHomonyms(IEnumerable<IAdTree> homonymAdTrees)
        {
            bool isAdded = false;

            // Consider primitive transferences for each homonym e.g. O>A.
            IEnumerable<IAdTree> homonymsAndPrimitiveTransferences = homonymAdTrees
                .SelectMany(x => new IAdTree[] { x }.Concat(GetPrimitiveTransferences(x)))
                .Distinct();

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                List<IAdTree> newListOfAdTrees = new List<IAdTree>();

                // Go via active adtrees.
                foreach (IAdTree activeAdTree in myActiveAdTrees)
                {
                    // Go via all homonyms and primitive transferences.
                    foreach (IAdTree newAdTree in homonymsAndPrimitiveTransferences)
                    {
                        // Try to append.
                        TryToAppend(activeAdTree, newAdTree, newListOfAdTrees);
                    }
                }

                if (newListOfAdTrees.Count > 0)
                {
                    myActiveAdTrees = newListOfAdTrees;
                    isAdded = true;
                }
            }
            // Create new adtrees.
            else
            {
                // Start active adtrees.
                myActiveAdTrees.AddRange(homonymsAndPrimitiveTransferences);

                if (myActiveAdTrees.Count > 0)
                {
                    isAdded = true;
                }
            }

            return isAdded;
        }


        private IEnumerable<IAdTree> GetAdTrees(IEnumerable<Morpheme> morphemes)
        {
            foreach (Morpheme morpheme in morphemes)
            {
                IEnumerable<Pattern> matchingPatterns = myConstructiveDictionary.FindPatterns(morpheme);

                // Go via patterns matching the incoming morpheme.
                foreach (Pattern pattern in matchingPatterns)
                {
                    // Create the adtree element from the incoming morpheme and its pattern.
                    IAdTree newAdTree = new AdTree(morpheme, pattern);
                    yield return newAdTree;
                }
            }
        }

        private IEnumerable<IAdTree> GetPrimitiveTransferences(IAdTree adTree)
        {
            IEnumerable<Pattern> patterns = myConstructiveDictionary.FindGrammarCharacterTransferencePatterns(adTree.Morpheme);
            foreach (Pattern pattern in patterns)
            {
                if (pattern.MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> isRule)
                {
                    IAdTree adTreeCopy = adTree.MakeShallowCopy();
                    IAdTree transferenceAdTree = new AdTree(new Morpheme("", isRule.ReferenceValue), pattern);
                    transferenceAdTree.Right = adTreeCopy;
                    yield return transferenceAdTree;
                }
            }
        }

        private void TryToAppend(IAdTree current, IAdTree newElement, List<IAdTree> results)
        {
            IAdTree placeToAppend = GetPlaceToAppend(current);

            // If an adposition needs to be filled.
            if (!placeToAppend.Pattern.MorphemeRule.Equals(MorphemeRule.Nothing) &&
                !placeToAppend.Pattern.MorphemeRule.Evaluate(placeToAppend.Morpheme))
            {
                if (placeToAppend.Pattern.Equals(newElement.Pattern))
                {
                    IAdTree newElementCopy = newElement.MakeShallowCopy();
                    IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                    adTreeCopy.Replace(newElementCopy);
                    results.Add(newElementCopy);
                }

                return;
            }

            bool isMorphematicAdPosition = newElement.Pattern.IsMorphematicAdPosition();
            bool canAppendIndirectly = CanAppendIndirectly(current) && !isMorphematicAdPosition;


            // If the new element can be attached to left.
            if (placeToAppend.Left == null && !placeToAppend.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
            {
                // Try to attach the new element directly.
                if (placeToAppend.CanAttachToLeft(newElement))
                {
                    IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                    IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                    adTreeCopy.Left = newAdTreeElementCopy;
                    results.Add(newAdTreeElementCopy);
                }
                if (canAppendIndirectly)
                {
                    // Try to attach the new element indirectly.
                    TryToAppendIndirectly(AttachingPosition.ParrentForLeft, placeToAppend, newElement, results, 2);
                }
            }

            // If the new element can be attached to right.
            if (placeToAppend.Right == null && !placeToAppend.Pattern.RightRule.Equals(MorphemeRule.Nothing))
            {
                // Try to attach the new element directly.
                if (placeToAppend.CanAttachToRight(newElement))
                {
                    IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                    IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                    adTreeCopy.Right = newAdTreeElementCopy;
                    results.Add(newAdTreeElementCopy);
                }
                if (canAppendIndirectly)
                {
                    // Try to attach the new element indirectly.
                    TryToAppendIndirectly(AttachingPosition.ParrentForRight, placeToAppend, newElement, results, 2);
                }
            }

            // If the new element can attach the placeToAppend to its left or right.
            // If the adtree has free adposition - i.e. it can be attached to left or right.
            if (placeToAppend.AdPosition == null)
            {
                // The new element tries to attach the adtree to its left.
                if (newElement.Left == null && !newElement.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
                {
                    if (newElement.CanAttachToLeft(placeToAppend))
                    {
                        IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        newAdTreeElementCopy.Left = adTreeCopy;
                        results.Add(adTreeCopy);
                    }
                    // Try to attach the adtree indirectly via new element's left.
                    else if (canAppendIndirectly)
                    {
                        TryToAppendIndirectly(AttachingPosition.ParrentForLeft, newElement, placeToAppend, results, 2);
                    }
                }

                // The new element tries to attach the adtree to its right.
                if (newElement.Right == null && !newElement.Pattern.RightRule.Equals(MorphemeRule.Nothing))
                {
                    if (newElement.CanAttachToRight(placeToAppend))
                    {
                        IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        newAdTreeElementCopy.Right = adTreeCopy;
                        results.Add(adTreeCopy);
                    }
                    // Try to attach the adtree indirectly via new element's right.
                    else if (canAppendIndirectly)
                    {
                        TryToAppendIndirectly(AttachingPosition.ParrentForRight, newElement, placeToAppend, results, 2);
                    }
                }

                if (canAppendIndirectly)
                {
                    // Also try to connect the adtree and the new element indirectly via adtree adposition.
                    TryToAppendIndirectly(AttachingPosition.ChildOnLeft, placeToAppend, newElement, results, 2);
                    TryToAppendIndirectly(AttachingPosition.ChildOnRight, placeToAppend, newElement, results, 2);
                }
            }
        }

        private void TryToAppendIndirectly(AttachingPosition startElementAppendPosition, IAdTree start, IAdTree end, List<IAdTree> results, int stopLevel)
        {
            if (stopLevel <= 0)
            {
                return;
            }
            --stopLevel;



            GrammarCharacter startGrammarCharacter = GrammarCharacter.e;
            GrammarCharacter endGrammarCharacter = GrammarCharacter.e;

            // If start adTree connects the bridge via its left.
            if (startElementAppendPosition == AttachingPosition.ParrentForLeft)
            {
                startGrammarCharacter = start.Pattern.LeftRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If start adTree connects the bridge via its right.
            else if (startElementAppendPosition == AttachingPosition.ParrentForRight)
            {
                startGrammarCharacter = start.Pattern.RightRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its left.
            else if (startElementAppendPosition == AttachingPosition.ChildOnLeft)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its right.
            else if (startElementAppendPosition == AttachingPosition.ChildOnRight)
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
                        if (startElementAppendPosition == AttachingPosition.ParrentForLeft)
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
                        else if (startElementAppendPosition == AttachingPosition.ParrentForRight)
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
                        else if (startElementAppendPosition == AttachingPosition.ChildOnLeft)
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
                        else if (startElementAppendPosition == AttachingPosition.ChildOnRight)
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
                            TryToAppendIndirectly(AttachingPosition.ParrentForLeft, bridge, endCopy, results, stopLevel);

                            bridge.Left = endCopy;
                            results.Add(endCopy);
                        }
                        else if (bridge.Right == null && bridge.CanAttachToRight(end))
                        {
                            TryToAppendIndirectly(AttachingPosition.ParrentForRight, bridge, endCopy, results, stopLevel);

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
