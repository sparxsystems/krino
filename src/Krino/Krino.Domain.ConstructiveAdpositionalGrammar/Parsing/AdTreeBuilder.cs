using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeBuilder
    {
        private IEnumerable<IPattern> myPatterns;
        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        public AdTreeBuilder(IEnumerable<IPattern> patterns)
        {
            myPatterns = patterns;
        }

        public IReadOnlyList<IAdTree> ActiveAdTrees => myActiveAdTrees;

        public bool AddMorpheme(IMorpheme morpheme)
        {
            bool isAdded = false;

            IEnumerable<IPattern> morphemeMatchingPatterns = GetApplicablePatterns(morpheme);

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                List<IAdTree> newListOfAdTrees = new List<IAdTree>();

                // Go via active adtrees (i.e. separate adtrees which are still candidates to become the result adtree).
                foreach(IAdTree adTreeRoot in myActiveAdTrees)
                {
                    // Go via patterns which match the incoming morpheme.
                    foreach (IPattern pattern in morphemeMatchingPatterns)
                    {
                        IAdTree adTreeCopy = adTreeRoot.MakeShallowCopy();

                        IAdTree newAdTreeElement = new AdTree()
                        {
                            Morpheme = morpheme,
                            Pattern = pattern,
                        };

                        // Attach the new adtree element to the adtree.
                        if (newAdTreeElement.GrammarCharacter == GrammarCharacter.U ||
                            newAdTreeElement.GrammarCharacter == GrammarCharacter.Epsilon)
                        {
                            newAdTreeElement.Left = adTreeCopy;
                        }
                        else if (adTreeCopy.Left == null)
                        {
                            adTreeCopy.Left = newAdTreeElement;
                        }
                        else if (adTreeCopy.Right == null)
                        {
                            adTreeCopy.Right = newAdTreeElement;
                        }

                        // Check if the attached tree element matches.
                        if (IsMatch(newAdTreeElement))
                        {
                            newListOfAdTrees.Add(newAdTreeElement.Root);
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
                foreach (IPattern pattern in morphemeMatchingPatterns)
                {
                    AdTree newAdTree = new AdTree()
                    {
                        Morpheme = morpheme,
                        Pattern = pattern
                    };

                    myActiveAdTrees.Add(newAdTree);
                }

                if (myActiveAdTrees.Count > 0)
                {
                    isAdded = true;
                }
            }


            return isAdded;
        }


        private IEnumerable<IPattern> GetApplicablePatterns(IMorpheme morpheme)
        {
            foreach (IPattern pattern in myPatterns)
            {
                if (pattern.MorphemeMatchingRule.IsMatch(morpheme.Morph, morpheme.Attributes))
                {
                    yield return pattern;
                }
            }
        }

        private bool IsMatch(IAdTree attachedAdTree)
        {
            bool isMatch = false;

            // The attachedAdtree is attached to the adposition.
            if (attachedAdTree.IsAdPosition)
            {
                // If it is attached via the left child.
                if (attachedAdTree.Left != null)
                {
                    // Check adposition->left connection.
                    isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree.Left, attachedAdTree.Pattern.LeftMatchingRule);
                    if (isMatch)
                    {
                        // Also check left->adposition connection.
                        isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree, attachedAdTree.Left.Pattern.AdPositionMatchingRule);
                    }
                }
                else if (attachedAdTree.Right != null)
                {
                    // Check adposition->right connection.
                    isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree.Right, attachedAdTree.Pattern.LeftMatchingRule);
                    if (isMatch)
                    {
                        // Also check right->adposition connection.
                        isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree, attachedAdTree.Right.Pattern.AdPositionMatchingRule);
                    }
                }
            }
            // The attachedAdtree is attached to the left child.
            else if (attachedAdTree.IsOnLeft)
            {
                // Check left->adposition connection.
                isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree.AdPosition, attachedAdTree.Pattern.AdPositionMatchingRule);
                if (isMatch)
                {
                    // Also check adposition->left connection.
                    isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree, attachedAdTree.AdPosition.Pattern.LeftMatchingRule);
                }

                // If the attachedAdTree is a dependent (i.e. not a "technical" adtree).
                if (isMatch && attachedAdTree.IsDependent)
                {
                    // Check the governor too.
                    IAdTree governor = attachedAdTree.Governor;
                    if (governor != null)
                    {
                        isMatch = IsMatchOfAdTreeAndPatternRule(governor, attachedAdTree.Pattern.GovernorMatchingRule);
                    }
                }
            }
            else if (attachedAdTree.IsOnRight)
            {
                // Check right->adposition connection.
                isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree.AdPosition, attachedAdTree.Pattern.AdPositionMatchingRule);
                if (isMatch)
                {
                    // Also check adposition->left connection.
                    isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree, attachedAdTree.AdPosition.Pattern.RightMatchingRule);
                }

                if (isMatch && attachedAdTree.IsGovernor)
                {
                    // Check if it matches with all dependents.
                    foreach (IAdTree dependent in attachedAdTree.DependentAdPositions)
                    {
                        isMatch = IsMatchOfAdTreeAndPatternRule(attachedAdTree, dependent.Pattern.GovernorMatchingRule);
                        if (!isMatch)
                        {
                            break;
                        }
                    }
                }
            }

            return isMatch;
        }

        private bool IsMatchOfAdTreeAndPatternRule(IAdTree adTree, PatternRule rule)
        {
            bool isMatch = rule.IsMatch(adTree.Morpheme.Morph, adTree.Morpheme.Attributes, adTree.Pattern.PatternAttributes);
            return isMatch;
        }
    }
}
