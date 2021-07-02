using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    internal static class PatternsExt
    {
        public static DirectedGraph<Pattern, AdTreePosition> CreatePatternGraph(this IEnumerable<Pattern> patterns)
        {
            using var _t = Trace.Entering();

            var result = new DirectedGraph<Pattern, AdTreePosition>();
            result.AddVertices(patterns);

            foreach (var fromPattern in patterns)
            {
                foreach (var toPattern in patterns)
                {
                    if (fromPattern.RightPatternRule.Evaluate(toPattern))
                    {
                        result.AddEdge(fromPattern, toPattern, AdTreePosition.ParrentForChildOnRight);
                    }

                    if (fromPattern.LeftPatternRule.Evaluate(toPattern))
                    {
                        result.AddEdge(fromPattern, toPattern, AdTreePosition.ParrentForChildOnLeft);
                    }
                }
            }

            return result;
        }
    }
}
