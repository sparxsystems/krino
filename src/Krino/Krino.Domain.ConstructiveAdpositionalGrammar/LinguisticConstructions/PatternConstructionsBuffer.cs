using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Threading;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    internal class PatternConstructionsBuffer
    {
        private DoubleKeyDictionary<string, Pattern, HashSet<AdTreeFactory>> myBuffer;

        public PatternConstructionsBuffer(IDirectedGraph<Pattern, AdTreePosition> patternGraph, int maxMorphemes)
        {
            MaxMorphemes = maxMorphemes;
            myBuffer = new DoubleKeyDictionary<string, Pattern, HashSet<AdTreeFactory>>();

            var allAdTreeFactories = patternGraph.GetAllAdTreeFactories(maxMorphemes);
            foreach (var factory in allAdTreeFactories)
            {
                var patternSignature = factory.PatternSignature;

                myBuffer.TryGetValue(patternSignature, factory.Pattern, out var storedFactories);
                if (storedFactories == null)
                {
                    storedFactories = new HashSet<AdTreeFactory>(new AdTreeFactoryComparer());
                    myBuffer[patternSignature, factory.Pattern] = storedFactories;
                }

                storedFactories.Add(factory);
            }

            Count = allAdTreeFactories.Count;
        }

        public int Count { get; private set; }

        public int MaxMorphemes { get; private set; }

        public HashSet<AdTreeFactory> GetAdTreeFactories(string patternSignature, Pattern startingPattern)
        {
            HashSet<AdTreeFactory> result = null;

            // If the buffer contains the result if exists.
            if (patternSignature.Length <= MaxMorphemes)
            {
                myBuffer.TryGetValue(patternSignature, startingPattern, out result);
            }

            return result;
        }
    }
}
