using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public class Morpheme : IMorpheme
    {
        public Morpheme (string morph)
        {
            Morph = morph;
        }

        public string Morph { get; private set; }

        public List<ISememe> Sememes { get; } = new List<ISememe>();

        public Dictionary<string, Attribute> Attributes { get; } = new Dictionary<string, Attribute>();
    }
}
