using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public class Morpheme
    {
        public Morpheme (Morph morph, List<ISememe> sememes)
        {
            Morph = morph;
            Sememes = sememes ?? new List<ISememe>();
        }

        public Morph Morph { get; private set; }

        public List<ISememe> Sememes { get; private set; }


    }
}
