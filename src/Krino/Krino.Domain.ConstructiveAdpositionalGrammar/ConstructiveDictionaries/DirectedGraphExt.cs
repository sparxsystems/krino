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
            var result = GetPossibleAdTreesIntern(patternGraph, start, attributesModel, maxMorphemes, maxMorphemes);
            return result;
        }

        public static IEnumerable<IAdTree> GetPossibleAdTreesIntern(IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes, int maxRecursion)
        {
            using var _t = Trace.Entering();

            if (start.IsLikeMorpheme)
            {
                yield return null;
            }
            else if (maxRecursion > 0)
            {
                // Get connecting patterns which are not morphemes.
                // Note: only one morpheme pattern is needed, othrwise the final result would be many same adtrees.
                var rightChildPatterns = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnRight)
                    .Select(x => x.To);
                var rightChildNonMorphemes = rightChildPatterns.Where(x => !x.IsLikeMorpheme);
                var rightChildMorpheme = rightChildPatterns.FirstOrDefault(x => x.IsLikeMorpheme);
                var rightChildRelevantPatterns = rightChildNonMorphemes;
                if (rightChildMorpheme != null)
                {
                    rightChildRelevantPatterns = rightChildNonMorphemes.Concat(new Pattern[] { rightChildMorpheme });
                }


                var leftChildPatterns = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnLeft)
                    .Select(x => x.To);
                var leftChildNonMorphemes = leftChildPatterns.Where(x => !x.IsLikeMorpheme);
                var leftChildMorpheme = leftChildPatterns.FirstOrDefault(x => x.IsLikeMorpheme);
                var leftChildRelevantPatterns = leftChildNonMorphemes;
                if (leftChildMorpheme != null)
                {
                    leftChildRelevantPatterns = leftChildRelevantPatterns.Concat(new Pattern[] { leftChildMorpheme });
                }

                var rightSubAdTrees = rightChildRelevantPatterns
                    .SelectMany(x => GetPossibleAdTreesIntern(patternGraph, x, attributesModel, maxMorphemes, maxRecursion - 1))
                    .ToList();

                var leftSubAdTrees = leftChildRelevantPatterns
                    .SelectMany(x => GetPossibleAdTreesIntern(patternGraph, x, attributesModel, maxMorphemes, maxRecursion - 1))
                    .ToList();

                if (rightSubAdTrees.Any() && leftSubAdTrees.Any())
                {
                    foreach (var rightSubAdTree in rightSubAdTrees)
                    {
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
                else if (rightSubAdTrees.Any())
                {
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
                else if (leftSubAdTrees.Any())
                {
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
