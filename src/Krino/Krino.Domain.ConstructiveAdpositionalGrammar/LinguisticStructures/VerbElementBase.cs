using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal abstract class VerbElementBase : PhraseBase, IVerbElement
    {
        public VerbElementBase(BigInteger attributes)
            : base(attributes)
        {
        }

        public IPhrase Verb => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Phrase.VerbPhrase.IsIn(x.Attributes));

        public IPhrase DirectObject => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Object.ObjectOfVerb.Direct.IsIn(x.Attributes));

        public IPhrase IndirectObject => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Object.ObjectOfVerb.Indirect.IsIn(x.Attributes));

        public IPhrase ObjectComplement => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.ObjectComplement.IsIn(x.Attributes));

        public IPhrase AdverbialComplement => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.AdverbialComplement.IsIn(x.Attributes));

        public IPhrase SubjectComplement => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.SubjectComplement.IsIn(x.Attributes));

        public IPhrase AdjectiveComplement => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.Complement.AdjectiveComplement.IsIn(x.Attributes));

        public IPhrase Adverbial => Items.OfType<IPhrase>().FirstOrDefault(x => GrammarAttributes.AdverbialAdjunct.IsIn(x.Attributes));
    }
}
