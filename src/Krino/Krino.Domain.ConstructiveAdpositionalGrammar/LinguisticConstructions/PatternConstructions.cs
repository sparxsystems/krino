using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    public class PatternConstructions
    {
        private DirectedGraph<Pattern, AdTreePosition> myPatternGraph;
        private PatternConstructionsBuffer myConstructionsBuffer;

        public PatternConstructions(int maxMorphemes, IEnumerable<Pattern> allPatterns)
        {
            myPatternGraph = allPatterns.CreatePatternGraph();
            myConstructionsBuffer = new PatternConstructionsBuffer(myPatternGraph, maxMorphemes);
        }

        public int Count => myConstructionsBuffer.Count;

        public IEnumerable<Pattern> AllPatterns => myPatternGraph;

        public IReadOnlyList<AdTreeFactory> GetPatternFactories(string patternSignature)
            => myPatternGraph.GetAdTreeFactoriesForPatternSignature(patternSignature, myConstructionsBuffer);
    }
}
