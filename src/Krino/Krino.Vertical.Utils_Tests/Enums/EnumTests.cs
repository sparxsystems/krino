using Krino.Vertical.Utils.Enums;
using NUnit.Framework;
using System;

namespace Krino.Vertical.Utils_Tests.Enums
{
    [TestFixture]
    public class EnumTests
    {
        public class DummyEnumRoot : EnumRootBase
        {
            public class DummyCategory1 : EnumGroupBase
            {
                public class DummyCategory12 : EnumGroupBase
                {
                    public DummyCategory12(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 3)
                    {
                        Attr111 = new EnumValue(this, 1);
                        Attr112 = new EnumValue(this, 2);
                        Attr113 = new EnumValue(this, 3);
                    }

                    public EnumValue Attr111 { get; }
                    public EnumValue Attr112 { get; }
                    public EnumValue Attr113 { get; }
                }

                public DummyCategory1(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 2)
                {
                    Category11 = new DummyCategory12(this, 1);
                    Val12 = new EnumValue(this, 2);
                }

                public DummyCategory12 Category11 { get; }
                public EnumValue Val12 { get; }
            }



            private DummyEnumRoot() : base(2) { }

            private static DummyEnumRoot Instance { get; } = new DummyEnumRoot();

            public static DummyCategory1 Category1 { get; } = new DummyCategory1(Instance, 1);

            public static EnumValue Val2 { get; } = new EnumValue(Instance, 2);

        }


        public class ExceedingLengthEnumRoot : EnumRootBase
        {
            public ExceedingLengthEnumRoot() : base(2)
            {
                // The group length is 2 but here are three enum properties.
                Attr1 = new EnumValue(this, 1);
                Attr2 = new EnumValue(this, 2);
                Attr3 = new EnumValue(this, 3);
            }

            public EnumValue Attr1 { get; }
            public EnumValue Attr2 { get; }
            public EnumValue Attr3 { get; }
        }


        [Test]
        public void EncodedValue_ulong()
        {
            // 10 10 100
            ulong encodedValue = DummyEnumRoot.Category1.Category11.Attr112;
            Assert.AreEqual(0xA4_00_00_00_00_00_00_00, encodedValue);

            // 10 01
            encodedValue = DummyEnumRoot.Category1.Val12;
            Assert.AreEqual(0x90_00_00_00_00_00_00_00, encodedValue);

            // 10 10 001
            encodedValue = DummyEnumRoot.Category1.Category11.Attr113;
            Assert.AreEqual(0xA2_00_00_00_00_00_00_00, encodedValue);
        }

        [Test]
        public void EncodedValue_comparing()
        {
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr111 > DummyEnumRoot.Category1.Category11.Attr112);
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr112 > DummyEnumRoot.Category1.Category11.Attr113);
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr113 > DummyEnumRoot.Category1.Category11);
            Assert.IsTrue(DummyEnumRoot.Category1.Category11 > DummyEnumRoot.Category1.Val12);
            Assert.IsTrue(DummyEnumRoot.Category1.Val12 > DummyEnumRoot.Category1);
            Assert.IsTrue(DummyEnumRoot.Category1 > DummyEnumRoot.Val2);
        }

        [Test]
        public void IsIn()
        {
            ulong encodedValue = DummyEnumRoot.Category1.Category11.Attr112 | DummyEnumRoot.Val2;

            Assert.IsTrue(DummyEnumRoot.Category1.IsIn(encodedValue));
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.IsIn(encodedValue));
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr112.IsIn(encodedValue));

            Assert.IsTrue(DummyEnumRoot.Val2.IsIn(encodedValue));

            Assert.IsFalse(DummyEnumRoot.Category1.Val12.IsIn(encodedValue));
            Assert.IsFalse(DummyEnumRoot.Category1.Category11.Attr111.IsIn(encodedValue));
            Assert.IsFalse(DummyEnumRoot.Category1.Category11.Attr113.IsIn(encodedValue));
        }

        [Test]
        public void ExceedingCapacity_of_Group()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => new ExceedingLengthEnumRoot());
        }

        public class DummyEnumRoot62 : EnumRootBase
        {
            public class DummyCategory2 : EnumGroupBase
            {
                public DummyCategory2(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 2) { }
            }

            private DummyEnumRoot62() : base(62) {}

            public static DummyEnumRoot62 Instance { get; } = new DummyEnumRoot62();

            public static DummyCategory2 Category2 { get; } = new DummyCategory2(Instance, 1);
        }

        public class DummyEnumRoot63 : EnumRootBase
        {
            public class DummyCategory2 : EnumGroupBase
            {
                public DummyCategory2(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 2) { }
            }

            private DummyEnumRoot63() : base(63) { }

            public static DummyEnumRoot63 Instance { get; } = new DummyEnumRoot63();

            public static DummyCategory2 Category2 { get; } = new DummyCategory2(Instance, 1);
        }

        public class DummyEnumRoot64 : EnumRootBase
        {
            public DummyEnumRoot64() : base(64) { }
        }

        public class DummyEnumRoot65 : EnumRootBase
        {
            public DummyEnumRoot65() : base(65) { }
        }

        [Test]
        public void Capacity_of_uint()
        {
            // 62 + 2 = 64 -> OK
            var _ = DummyEnumRoot62.Category2;

            // 64 -> OK
            new DummyEnumRoot64();

            // 63 + 2 = 65 -> exception
            bool isRightException = false;
            try
            {
                var a1 = DummyEnumRoot63.Category2;
            }
            catch (TypeInitializationException err)
            {
                isRightException = err.InnerException is ArgumentOutOfRangeException;
            }
            Assert.IsTrue(isRightException);


            // 65 -> exception
            Assert.Throws<ArgumentOutOfRangeException>(() => new DummyEnumRoot65());
        }
    }
}
