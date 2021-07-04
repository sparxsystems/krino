using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    internal static class DirectedGraphExt
    {
        public static IEnumerable<IAdTree> GetPossibleAdTrees(this IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes)
        {
            var result = GetPossibleAdTreesIntern(patternGraph, start, attributesModel, maxMorphemes);
            return result;
        }

        public static IEnumerable<IAdTree> GetPossibleAdTreesIntern(IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes)
        {
            using var _t = Trace.Entering();

            if (start.IsLikeMorpheme)
            {
                yield return null;
            }
            else if (maxMorphemes >= 2)
            {
                // Get connecting non-morpheme patterns + 1 morpheme pattern.
                // Note: only one morpheme pattern is needed, otherwise the final result would be many same adtrees.
                var patternsOnRight = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnRight)
                    .Select(x => x.To);
                var nonMorphemePatternsOnRight = patternsOnRight.Where(x => !x.IsLikeMorpheme);
                var morphemePatternOnRight = patternsOnRight.FirstOrDefault(x => x.IsLikeMorpheme);
                var relevantPatternsOnRight = nonMorphemePatternsOnRight;
                if (morphemePatternOnRight != null)
                {
                    relevantPatternsOnRight = new Pattern[] { morphemePatternOnRight }.Concat(nonMorphemePatternsOnRight);
                }


                var patternsOnLeft = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnLeft)
                    .Select(x => x.To);
                var nonMorphemePatternsOnLeft = patternsOnLeft.Where(x => !x.IsLikeMorpheme);
                var morphemePatternOnLeft = patternsOnLeft.FirstOrDefault(x => x.IsLikeMorpheme);
                var relevantPatternsOnLeft = nonMorphemePatternsOnLeft;
                if (morphemePatternOnLeft != null)
                {
                    relevantPatternsOnLeft = new Pattern[] { morphemePatternOnLeft }.Concat(nonMorphemePatternsOnLeft);
                }

                if (relevantPatternsOnRight.Any() && relevantPatternsOnLeft.Any())
                {
                    foreach (var rightPattern in relevantPatternsOnRight)
                    {
                        var rightSubAdTrees = GetPossibleAdTreesIntern(patternGraph, rightPattern, attributesModel, maxMorphemes - 1);
                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            // Note: if rightPattern is a morpheme then the rightSubAdTree is null.

                            var morphemesOnRight = rightSubAdTree != null ? GetMorphemesCount(rightSubAdTree) : 1;

                            if (morphemesOnRight < maxMorphemes)
                            {
                                foreach (var leftPattern in relevantPatternsOnLeft)
                                {
                                    var newMaxMorphemesOnLeft = maxMorphemes - morphemesOnRight;
                                    var leftSubAdTrees = GetPossibleAdTreesIntern(patternGraph, leftPattern, attributesModel, newMaxMorphemesOnLeft);
                                    foreach (var leftSubAdTree in leftSubAdTrees)
                                    {
                                        var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                                        adTree.Right = rightSubAdTree;
                                        adTree.Left = leftSubAdTree;

                                        var morphemesCount = GetMorphemesCount(adTree);
                                        if (morphemesCount <= maxMorphemes)
                                        {
                                            yield return adTree;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                else if (relevantPatternsOnRight.Any())
                {
                    foreach (var rightPattern in relevantPatternsOnRight)
                    {
                        var rightSubAdTrees = GetPossibleAdTreesIntern(patternGraph, rightPattern, attributesModel, maxMorphemes - 1);
                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                            adTree.Right = rightSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                yield return adTree;
                            }
                        }
                    }
                }
                else if (relevantPatternsOnLeft.Any())
                {
                    foreach (var leftPattern in relevantPatternsOnLeft)
                    {
                        var leftSubAdTrees = GetPossibleAdTreesIntern(patternGraph, leftPattern, attributesModel, maxMorphemes - 1);
                        foreach (var leftSubAdTree in leftSubAdTrees)
                        {
                            var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                            adTree.Left = leftSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                yield return adTree;
                            }
                        }
                    }
                }
            }
        }

        private static int GetMorphemesCount(IAdTree adTree)
        {
            var result = adTree.Root.Sum(x =>
            {
                int count = 0;
                if (x.Left == null)
                {
                    ++count;
                }
                if (x.Right == null)
                {
                    ++count;
                }
                if (x.Pattern.IsMorphematicAdPosition())
                {
                    ++count;
                }
                return count;
            });
            return result;
        }

    }
}
