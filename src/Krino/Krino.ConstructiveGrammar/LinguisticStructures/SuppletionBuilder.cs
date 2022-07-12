using System.Collections.Generic;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public class SuppletionBuilder
    {
        private List<Suppletion> mySuppletion;

        public SuppletionBuilder Add(string value, BigInteger attributes)
        {
            mySuppletion ??= new List<Suppletion>();

            mySuppletion.Add(new Suppletion(value, attributes));
            return this;
        }

        public List<Suppletion> Tolist() => mySuppletion;
    }
}
