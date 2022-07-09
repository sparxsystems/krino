using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.Morphology;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Dictionary
{
    public class DictionaryEntry
    {
        private IMorpheme myMorpheme;
        private List<IMorphemeBinding> myTransforms;

        public DictionaryEntry(IMorpheme morpheme, params IMorphemeBinding[] transforms)
        {
            myMorpheme = morpheme;
            myTransforms = transforms?.ToList() ?? new List<IMorphemeBinding>();
        }

        public IMorpheme Morpheme => myMorpheme;
        public List<IMorphemeBinding> Transforms => myTransforms;
    }
}
