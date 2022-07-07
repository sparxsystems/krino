using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using System.Linq;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    internal abstract class VerbElementBase : PhraseBase, IVerbElement
    {
        public VerbElementBase(BigInteger attributes)
            : base(attributes)
        {
        }

        public IPhrase Verb => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Phrase.VerbPhrase.IsIn(x.Attributes));

        public IPhrase DirectObject => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Object.ObjectOfVerb.Direct.IsIn(x.Attributes));

        public IPhrase IndirectObject => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Object.ObjectOfVerb.Indirect.IsIn(x.Attributes));

        public IPhrase ObjectComplement => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.ObjectComplement.IsIn(x.Attributes));

        public IPhrase AdverbialComplement => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.AdverbialComplement.IsIn(x.Attributes));

        public IPhrase SubjectComplement => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.SubjectComplement.IsIn(x.Attributes));

        public IPhrase AdjectiveComplement => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.AdjectiveComplement.IsIn(x.Attributes));

        public IPhrase AdverbialAdjunct => DirectItems.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.AdverbialAdjunct.IsIn(x.Attributes));
    }
}
