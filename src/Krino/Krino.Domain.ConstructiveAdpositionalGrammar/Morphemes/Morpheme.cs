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

        /// <summary>
        /// Grammar character.
        /// </summary>
        public GrammarCharacter GrammarCharacter { get; set; }

        /// <summary>
        /// In case of actant it specifies the valency it saturates.
        /// </summary>
        public int SaturatedValency { get; set; }

        /// <summary>
        /// In case of Verbant defines the number of expected dependents.
        /// </summary>
        public int Valency { get; set; }

        public Morph Morph { get; private set; }

        public List<ISememe> Sememes { get; private set; }


    }
}
