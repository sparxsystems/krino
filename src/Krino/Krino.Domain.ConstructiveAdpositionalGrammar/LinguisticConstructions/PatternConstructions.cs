using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    public class PatternConstructions
    {
        //public PatternConstructions(int maxMorphemes, IEnumerable<Pattern> allPatterns, IEnumerable<Pattern> rootPatterns)
        //{
        //    AllPatterns = allPatterns;

        //    var patternFactories = new Dictionary<string, IReadOnlyList<AdTreeFactory>>();

        //    var patternGraph = allPatterns.CreatePatternGraph();

        //    foreach (var pattern in rootPatterns)
        //    {
        //        var factories = patternGraph.GetAdTreeFactories(pattern, maxMorphemes);

        //        foreach (var factory in factories)
        //        {
        //            var patternSignature = factory.PatternSignature;

        //            patternFactories.TryGetValue(patternSignature, out var storedFactories);
        //            if (storedFactories == null)
        //            {
        //                storedFactories = new List<AdTreeFactory>();
        //                patternFactories[patternSignature] = storedFactories;
        //            }

        //            ((List<AdTreeFactory>)storedFactories).Add(factory);
        //            ++Count;
        //        }
        //    }

        //    PatternFactories = patternFactories;
        //}

        public PatternConstructions(int maxMorphemes, IEnumerable<Pattern> allPatterns)
        {
            AllPatterns = allPatterns;

            var patternFactories = new Dictionary<string, IReadOnlyList<AdTreeFactory>>();

            var patternGraph = allPatterns.CreatePatternGraph();

            var factories = patternGraph.GetAdTreeFactories(allPatterns, maxMorphemes);

            foreach (var factory in factories)
            {
                var patternSignature = factory.PatternSignature;

                patternFactories.TryGetValue(patternSignature, out var storedFactories);
                if (storedFactories == null)
                {
                    storedFactories = new List<AdTreeFactory>();
                    patternFactories[patternSignature] = storedFactories;
                }

                ((List<AdTreeFactory>)storedFactories).Add(factory);
                ++Count;
            }

            PatternFactories = patternFactories;
        }

        public int Count { get; private set; }

        public IReadOnlyDictionary<string, IReadOnlyList<AdTreeFactory>> PatternFactories { get; private set; }

        public IEnumerable<Pattern> AllPatterns { get; private set; }
    }
}
