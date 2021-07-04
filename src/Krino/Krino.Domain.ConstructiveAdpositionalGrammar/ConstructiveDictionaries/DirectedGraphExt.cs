using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    internal static class DirectedGraphExt
    {
        public static IEnumerable<IAdTree> GetPossibleAdTrees(this IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes)
        {
            var buffer = new DoubleKeyDictionary<int, Pattern, IEnumerable<IAdTree>>();
            var result = GetPossibleAdTreesParallel(patternGraph, start, attributesModel, maxMorphemes, buffer);
            return result;
        }

        private static IEnumerable<IAdTree> GetPossibleAdTreesIntern(IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes)
        {
            using var _t = Trace.Entering();

            if (start.IsLikeMorpheme)
            {
                yield return null;
            }
            // If non-morpheme pattern is still possible - it connects two morphemes, left and right.
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

        private static IEnumerable<IAdTree> GetPossibleAdTreesParallel(IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            Pattern start, IAttributesModel attributesModel, int maxMorphemes,
            DoubleKeyDictionary<int, Pattern, IEnumerable<IAdTree>> buffer)
        {
            using var _t = Trace.Entering();

            var result = new List<IAdTree>();

            if (start.IsLikeMorpheme)
            {
                // Indicate the morpheme - end of the tree.
                result.Add(null);
            }
            // If non-morpheme pattern is still possible - it connects two morphemes, left and right.
            else if (maxMorphemes >= 2)
            {
                var parallelOptions = new ParallelOptions();
                if (maxMorphemes < 5)
                {
                    parallelOptions.MaxDegreeOfParallelism = 1;
                }

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
                    Parallel.ForEach(relevantPatternsOnRight, parallelOptions, rightPattern =>
                    {
                        var rightSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, rightPattern, attributesModel, maxMorphemes - 1);

                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            // Note: if rightPattern is a morpheme then the rightSubAdTree is null.

                            var morphemesOnRight = rightSubAdTree != null ? GetMorphemesCount(rightSubAdTree) : 1;

                            if (morphemesOnRight < maxMorphemes)
                            {
                                Parallel.ForEach(relevantPatternsOnLeft, parallelOptions, leftPattern =>
                                {
                                    var newMaxMorphemesOnLeft = maxMorphemes - morphemesOnRight;
                                    var leftSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, leftPattern, attributesModel, newMaxMorphemesOnLeft);
                                    
                                    foreach (var leftSubAdTree in leftSubAdTrees)
                                    {
                                        var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                                        adTree.Right = rightSubAdTree;
                                        adTree.Left = leftSubAdTree;

                                        var morphemesCount = GetMorphemesCount(adTree);
                                        if (morphemesCount <= maxMorphemes)
                                        {
                                            lock (result)
                                            {
                                                result.Add(adTree);
                                            }
                                        }
                                    }
                                });
                            }
                        }
                    });

                    return result;
                }
                else if (relevantPatternsOnRight.Any())
                {
                    Parallel.ForEach(relevantPatternsOnRight, parallelOptions, rightPattern =>
                    {
                        var rightSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, rightPattern, attributesModel, maxMorphemes - 1);
                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                            adTree.Right = rightSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                lock (result)
                                {
                                    result.Add(adTree);
                                }
                            }
                        }
                    });
                }
                else if (relevantPatternsOnLeft.Any())
                {
                    Parallel.ForEach(relevantPatternsOnLeft, parallelOptions, leftPattern =>
                    {
                        var leftSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, leftPattern, attributesModel, maxMorphemes - 1);
                        foreach (var leftSubAdTree in leftSubAdTrees)
                        {
                            var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                            adTree.Left = leftSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                lock (result)
                                {
                                    result.Add(adTree);
                                }
                            }
                        }
                    });
                }
            }

            return result;
        }

        private static IEnumerable<IAdTree> GetAdTreesFromBuffer(DoubleKeyDictionary<int, Pattern, IEnumerable<IAdTree>> buffer,
            IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            Pattern pattern,
            IAttributesModel attributesModel, int maxMorphemes)
        {
            IEnumerable<IAdTree> result = null;
            lock (buffer)
            {
                if (buffer.TryGetValue(maxMorphemes, pattern, out var adTrees))
                {
                    result = adTrees.ToList();
                }
            }

            if (result == null)
            {
                var adTrees = GetPossibleAdTreesParallel(patternGraph, pattern, attributesModel, maxMorphemes, buffer);

                lock (buffer)
                {
                    buffer[maxMorphemes, pattern] = adTrees;
                    result = adTrees.ToList();
                }
            }

            return result.Select(x => x?.MakeShallowCopy());
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
