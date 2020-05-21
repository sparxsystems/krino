using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using System;
using System.Collections.Generic;
using System.Linq;

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
        /// Keeps only one the most meaningful adtree.
        /// </summary>
        public void Collapse()
        {
            List<IAdTree> adTrees = new List<IAdTree>();

            List<Tuple<int, IAdTree>> ordered = myActiveAdTrees.Select(x => Tuple.Create(x.Root.GetNonconformities().Count(), x))
                .OrderBy(x => x.Item1)
                .ToList();

            if (ordered.Count > 1)
            {

            }

            Tuple<int, IAdTree> result = ordered.FirstOrDefault();
            if (result != null)
            {
                adTrees.Add(result.Item2);
            }

            myActiveAdTrees = adTrees;
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

            IEnumerable<IReadOnlyList<Morpheme>> morphemeSequences = myConstructiveDictionary.FindMorphemeSequences(word, maxMorphDistance);

            // Go via sequences.
            foreach (IReadOnlyList<Morpheme> sequence in morphemeSequences)
            {
                bool isCancelled = false;
                Morpheme lexeme = null;
                AdTreeBuilder localAdTreeBuilder = new AdTreeBuilder(myConstructiveDictionary);
                foreach (Morpheme morpheme in sequence)
                {
                    if (morpheme.IsLexeme)
                    {
                        if (lexeme != null)
                        {
                            if (!localAdTreeBuilder.AddMorpheme(lexeme))
                            {
                                isCancelled = true;
                                break;
                            }
                        }

                        lexeme = morpheme;
                    }
                    else
                    {
                        if (!localAdTreeBuilder.AddMorpheme(morpheme))
                        {
                            isCancelled = true;
                            break;
                        }
                    }
                }

                if (!isCancelled)
                {
                    if (lexeme != null)
                    {
                        if (!localAdTreeBuilder.AddMorpheme(lexeme))
                        {
                            isCancelled = true;
                        }
                    }

                    if (!isCancelled)
                    {
                        adTreesToAdd.AddRange(localAdTreeBuilder.ActiveAdTrees);
                    }
                }
            }

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

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                List<IAdTree> newListOfAdTrees = new List<IAdTree>();

                foreach (IAdTree activeAdTree in myActiveAdTrees)
                {
                    foreach (IAdTree newAdTree in homonymAdTrees)
                    {
                        TryToAppend(activeAdTree, newAdTree, newListOfAdTrees);

                        // Try to incorporate possible grammar character transferences.
                        IEnumerable<IAdTree> transferences = GetTransferences(newAdTree);
                        foreach (IAdTree transference in transferences)
                        {
                            TryToAppend(activeAdTree, transference, newListOfAdTrees);
                        }
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
                myActiveAdTrees.AddRange(homonymAdTrees);

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
                IEnumerable<Pattern> matchingPatterns = myConstructiveDictionary.FindMatchingPatterns(morpheme);

                // Go via patterns matching the incoming morpheme.
                foreach (Pattern pattern in matchingPatterns)
                {
                    // Create the adtree element from the incoming morpheme and its pattern.
                    IAdTree newAdTree = new AdTree(morpheme, pattern);
                    yield return newAdTree;
                }
            }
        }

        private IEnumerable<IAdTree> GetTransferences(IAdTree adTree)
        {
            // Try to incorporate possible grammar character transferences.
            IEnumerable<Pattern> transferencePatterns = myConstructiveDictionary.FindTransferencePatterns(adTree);
            foreach (Pattern transferencePattern in transferencePatterns)
            {
                IAdTree adTreeCopy = adTree.MakeShallowCopy();
                IAdTree transferenceAdTree = new AdTree(new Morpheme(""), transferencePattern);
                transferenceAdTree.Left = adTreeCopy;
                transferenceAdTree.Right = new AdTree(
                    new Morpheme("") { Attributes = transferenceAdTree.Pattern.RightRule.MorphemeRule.GrammarCharacter.GetAttributes() },
                    new Pattern(transferenceAdTree.Pattern.RightRule.MorphemeRule.GrammarCharacter.ToString())
                    {
                        MorphemeRule = new MorphemeRule(transferenceAdTree.Pattern.RightRule.MorphemeRule.GrammarCharacter, MorphRuleMaker.EmptyString, MaskRule.Is(transferenceAdTree.Pattern.RightRule.MorphemeRule.GrammarCharacter.GetAttributes())),
                        LeftRule = PatternRule.Nothing,
                        RightRule = PatternRule.Nothing,
                    });
                yield return transferenceAdTree;
            }
        }

        private void TryToAppend(IAdTree current, IAdTree newElement, List<IAdTree> results)
        {
            if (newElement.Morpheme.GrammarCharacter == GrammarCharacter.U)
            {
                int previousCount = results.Count;
                
                if (newElement.Right == null)
                {
                    TryToInsertAdPosition(AttachPosition.ParrentForRight, current, newElement, results);
                }

                if (previousCount == results.Count && newElement.Left == null)
                {
                    TryToInsertAdPosition(AttachPosition.ParrentForLeft, current, newElement, results);
                }
            }
            else
            {
                IAdTree placeToAppend = GetPlaceToAppend(current);

                // If the new element can be attached to left.
                if (placeToAppend.Left == null && !placeToAppend.Pattern.LeftRule.Equals(PatternRule.Nothing))
                {
                    // Try to attach the new element directly.
                    if (placeToAppend.CanAttachToLeft(newElement))
                    {
                        IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        adTreeCopy.Left = newAdTreeElementCopy;
                        results.Add(newAdTreeElementCopy);
                    }
                    // Try to attach the new element indirectly.
                    else
                    {
                        TryToAppendIndirectly(AttachPosition.ParrentForLeft, placeToAppend, newElement, results);
                    }
                }

                // If the new element can be attached to right.
                if (placeToAppend.Right == null && !placeToAppend.Pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    // Try to attach the new element directly.
                    if (placeToAppend.CanAttachToRight(newElement))
                    {
                        IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                        IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                        adTreeCopy.Right = newAdTreeElementCopy;
                        results.Add(newAdTreeElementCopy);
                    }
                    // Try to attach the new element indirectly.
                    else
                    {
                        TryToAppendIndirectly(AttachPosition.ParrentForRight, placeToAppend, newElement, results);
                    }
                }

                // If the new element can attach the placeToAppend to its left or right.
                // If the adtree has free adposition - i.e. it can be attached to left or right.
                if (placeToAppend.AdPosition == null)
                {
                    // The new element tries to attach the adtree to its left.
                    if (newElement.Left == null)
                    {
                        if (newElement.CanAttachToLeft(placeToAppend))
                        {
                            IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                            IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                            newAdTreeElementCopy.Left = adTreeCopy;
                            results.Add(adTreeCopy);
                        }
                        // Try to attach the adtree indirectly via new element's left.
                        else
                        {
                            TryToAppendIndirectly(AttachPosition.ParrentForLeft, newElement, placeToAppend, results);
                        }
                    }

                    // The new element tries to attach the adtree to its right.
                    if (newElement.Right == null)
                    {
                        if (newElement.CanAttachToRight(placeToAppend))
                        {
                            IAdTree newAdTreeElementCopy = newElement.MakeShallowCopy();
                            IAdTree adTreeCopy = placeToAppend.MakeShallowCopy();
                            newAdTreeElementCopy.Right = adTreeCopy;
                            results.Add(adTreeCopy);
                        }
                        // Try to attach the adtree indirectly via new element's right.
                        else
                        {
                            TryToAppendIndirectly(AttachPosition.ParrentForRight, newElement, placeToAppend, results);
                        }
                    }

                    // Also try to connect the adtree and the new element indirectly via adtree adposition.
                    TryToAppendIndirectly(AttachPosition.ChildOnLeft, placeToAppend, newElement, results);
                    TryToAppendIndirectly(AttachPosition.ChildOnRight, placeToAppend, newElement, results);
                }
            }
        }

        private void TryToInsertAdPosition(AttachPosition position, IAdTree current, IAdTree adPositionToInsert, List<IAdTree> results)
        {
            IEnumerable<IAdTree> adTreesToRoot = current.GetSequenceToRoot();

            foreach (IAdTree pathItem in adTreesToRoot)
            {
                // If adpositon shall attach it to left.
                if (position == AttachPosition.ParrentForLeft)
                {
                    if (adPositionToInsert.Left == null && adPositionToInsert.CanAttachToLeft(pathItem))
                    {
                        IAdTree adPositionToInsertCopy = adPositionToInsert.MakeShallowCopy();
                        IAdTree adTreeCopy = pathItem.MakeShallowCopy();

                        if (pathItem.IsOnLeft)
                        {
                            adTreeCopy.AdPosition.Left = adPositionToInsertCopy;
                        }
                        else if (pathItem.IsOnRight)
                        {
                            // Include all modifiers (e.g. adjectives) too.
                            adTreeCopy = adTreeCopy.GetFirstAdPositionOnLeft();

                            // Note: the insertion place is on left
                            adTreeCopy.AdPosition.Left = adPositionToInsertCopy;
                        }

                        adPositionToInsertCopy.Left = adTreeCopy;
                        results.Add(adTreeCopy);
                        break;
                    }
                }
                // If adpositon shall attach it to right.
                else if (position == AttachPosition.ParrentForRight)
                {
                    if (adPositionToInsert.Right == null && adPositionToInsert.CanAttachToRight(pathItem))
                    {
                        IAdTree adPositionToInsertCopy = adPositionToInsert.MakeShallowCopy();
                        IAdTree adTreeCopy = pathItem.MakeShallowCopy();

                        if (pathItem.IsOnLeft)
                        {
                            adTreeCopy.AdPosition.Left = adPositionToInsertCopy;
                        }
                        else if (pathItem.IsOnRight)
                        {
                            // Include all modifiers (e.g. adjectives) too.
                            adTreeCopy = adTreeCopy.GetFirstAdPositionOnLeft();

                            // Note: the insertion place is on left
                            adTreeCopy.AdPosition.Left = adPositionToInsertCopy;
                        }

                        adPositionToInsertCopy.Right = adTreeCopy;
                        results.Add(adTreeCopy);

                        break;
                    }
                }
            }
        }

        private void TryToAppendIndirectly(AttachPosition startElementAppendPosition, IAdTree start, IAdTree end, List<IAdTree> results)
        {
            GrammarCharacter startGrammarCharacter = GrammarCharacter.Epsilon;
            GrammarCharacter endGrammarCharacter = GrammarCharacter.Epsilon;

            // If start adTree connects the bridge via its left.
            if (startElementAppendPosition == AttachPosition.ParrentForLeft)
            {
                startGrammarCharacter = start.Pattern.LeftRule.MorphemeRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If start adTree connects the bridge via its right.
            else if (startElementAppendPosition == AttachPosition.ParrentForRight)
            {
                startGrammarCharacter = start.Pattern.RightRule.MorphemeRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its left.
            else if (startElementAppendPosition == AttachPosition.ChildOnLeft)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.MorphemeRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its right.
            else if (startElementAppendPosition == AttachPosition.ChildOnRight)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.Morpheme.GrammarCharacter, start.Pattern.RightRule.MorphemeRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.Morpheme.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }

            // Get possibile ways how to connect new element.
            IEnumerable<IReadOnlyList<DirectedEdge<Pattern>>> connectionPaths = myConstructiveDictionary.PatternGraph
                .FindAllPaths(startGrammarCharacter.ToString(), endGrammarCharacter.ToString());

            // Go via all possible ways.
            foreach (IReadOnlyList<DirectedEdge<Pattern>> path in connectionPaths)
            {
                // If all elements is possible to create without morphs.
                if (path.All(x => x.Value.MorphemeRule.MorphRule.Evaluate("")))
                {
                    IAdTree startCopy = start.MakeShallowCopy();
                    IAdTree endCopy = end.MakeShallowCopy();

                    IAdTree previousBridge = null;

                    // Go via the path.
                    for (int i = 0; i < path.Count; ++i)
                    {
                        DirectedEdge<Pattern> edge = path[i];

                        IAdTree bridge = new AdTree(new Morpheme("") { Attributes = edge.Value.MorphemeRule.GrammarCharacter.GetAttributes() }, edge.Value);

                        // If it is the first item on the path.
                        if (i == 0)
                        {
                            if (startElementAppendPosition == AttachPosition.ParrentForLeft)
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
                            else if (startElementAppendPosition == AttachPosition.ParrentForRight)
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
                            else if (startElementAppendPosition == AttachPosition.ChildOnLeft)
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
                            else if (startElementAppendPosition == AttachPosition.ChildOnRight)
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
                                // Note: this should never happen but do not throw exception.
                                //       It is not worth to interrupt krino.
                                break;
                            }
                        }

                        // If it is the last item in the path.
                        if (i == path.Count - 1)
                        {
                            if (bridge.Left == null && bridge.CanAttachToLeft(end))
                            {
                                bridge.Left = endCopy;
                                results.Add(endCopy);
                            }
                            else if (bridge.Right == null && bridge.CanAttachToRight(end))
                            {
                                bridge.Right = endCopy;
                                results.Add(endCopy);
                            }
                            else
                            {
                                // Note: this should never happen but do not throw exception.
                                //       It is not worth to interrupt krino.
                                break;
                            }
                        }

                        previousBridge = bridge;
                    }
                }
            }
        }

        private IAdTree GetPlaceToAppend(IAdTree current)
        {
            IEnumerable<IAdTree> adTrees = new IAdTree[] { current }.Concat(current.AdPositions);
            foreach (IAdTree adTree in adTrees)
            {
                if (adTree.Left == null && !adTree.Pattern.LeftRule.Equals(PatternRule.Nothing) ||
                    adTree.Right == null && !adTree.Pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    return adTree;
                }
            }

            return current.Root;
        }

        private GrammarCharacter ChooseGrammarCharacter(params GrammarCharacter[] grammarCharacters)
        {
            GrammarCharacter result = GrammarCharacter.Epsilon;

            foreach (GrammarCharacter grammarCharacter in grammarCharacters)
            {
                if (grammarCharacter != GrammarCharacter.Epsilon)
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
