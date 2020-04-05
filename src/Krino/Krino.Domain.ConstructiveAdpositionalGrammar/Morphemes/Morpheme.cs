using Krino.Vertical.Utils.Collections;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public class Morpheme
    {
        public Morpheme (Morph morph, ITree<Sememe> sememes)
        {
            Morph = morph;
            Sememes = sememes;
        }

        public Morph Morph { get; private set; }

        public ITree<Sememe> Sememes { get; private set; }


    }
}
