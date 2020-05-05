using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    public class PronounSememeAttribute : EnumGroupBase
    {
        public PronounSememeAttribute(EnumGroupBase parent) : base(parent)
        {
            Person = new PersonAttributes(this);
            Gender = new GenderAttributes(this);
            Number = new NumberAttributes(this);
            Case = new CaseAttributes(this);
        }

        public PersonAttributes Person { get; }

        public GenderAttributes Gender { get; }

        public NumberAttributes Number { get; }

        public CaseAttributes Case { get; }
    }
}
