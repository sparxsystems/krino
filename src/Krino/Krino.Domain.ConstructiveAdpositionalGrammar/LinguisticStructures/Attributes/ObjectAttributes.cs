using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class ObjectAttributes : EnumGroupBase
    {
        public ObjectAttributes(EnumGroupBase parent) : base(parent)
        {
            ObjectOfVerb = new ObjectOfVerbAttributes(this);
            ObjectOfPreposition = new EnumValue(this);
        }

        public ObjectOfVerbAttributes ObjectOfVerb { get; }

        public EnumValue ObjectOfPreposition { get; }
    }
}
