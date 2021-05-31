using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class TermAttributes : EnumGroupBase
    {
        public TermAttributes(EnumGroupBase parent) : base(parent)
        {
            Subject = new EnumValue(this);
            Predicate = new EnumValue(this);
        }

        public EnumValue Subject { get; }
        public EnumValue Predicate { get; }
    }
}
