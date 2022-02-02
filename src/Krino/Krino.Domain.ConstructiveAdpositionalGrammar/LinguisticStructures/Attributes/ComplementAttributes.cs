using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class ComplementAttributes : EnumGroupBase
    {
        public ComplementAttributes(EnumGroupBase parent) : base(parent)
        {
            SubjectComplement = new EnumValue(this);
            ObjectComplement = new EnumValue(this);
            AdjectiveComplement = new EnumValue(this);
            AdverbialComplement = new EnumValue(this);
        }

        public EnumValue SubjectComplement { get; }
        public EnumValue ObjectComplement { get; }
        public EnumValue AdjectiveComplement { get; }

        public EnumValue AdverbialComplement { get; }
    }
}
