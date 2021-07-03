using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    internal static class DirectedGraphExt
    {
        public static IEnumerable<IAdTree> GetPossibleAdTrees(this DirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes)
        {
            var result = GetPossibleAdTreesIntern(patternGraph, start, attributesModel, maxMorphemes, maxMorphemes);
            return result;
        }

        public static IEnumerable<IAdTree> GetPossibleAdTreesIntern(DirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes, int maxRecursion)
        {
            if (start.IsLikeMorpheme)
            {
                var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                yield return adTree;
            }
            else if (maxRecursion > 0)
            {
                var rightChildPatterns = patternGraph.GetEdgesGoingFrom(start)
                .Where(x => x.Value == AdTreePosition.ParrentForChildOnRight)
                .Select(x => x.To);

                var leftChildPatterns = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnLeft)
                    .Select(x => x.To);

                var rightSubAdTrees = rightChildPatterns
                    .SelectMany(x => GetPossibleAdTreesIntern(patternGraph, x, attributesModel, maxMorphemes, maxRecursion - 1))
                    .ToList();

                var leftSubAdTrees = leftChildPatterns
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
            var result = adTree.Root.Count(x => x.Pattern.IsLikeMorpheme || x.Pattern.IsMorphematicAdPosition());
            return result;
        }

    }
}
