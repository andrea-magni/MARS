/*** ROLES ***/
INSERT INTO ROLES ([NAME], DESCRIPTION) VALUES
('ADMIN',    'Full system administrator with all privileges'),
('MANAGER',  'Can manage customers and invoices'),
('VIEWER',   'Read-only access to all data'),
('BILLING',  'Can create and edit invoices only'),
('SUPPORT',  'Customer support access');
GO

/*** CUSTOMERS ***/
INSERT INTO CUSTOMER ([NAME], VAT_ID, COUNTRY) VALUES
('Mustermann GmbH',          'DE123456789',  'DE'),
('Schneider AG',             'DE987654321',  'DE'),
('Dupont S.A.S.',            'FR445566778',  'FR'),
('Acme Corp.',               'US001122334',  'US'),
('Tech Solutions Ltd.',      'GB556677889',  'GB'),
('Rossi S.r.l.',             'IT778899001',  'IT'),
('Van den Berg B.V.',        'NL334455667',  'NL'),
('Kowalski Sp. z o.o.',      'PL112233445',  'PL'),
('Nordic Supplies AB',       'SE667788990',  'SE'),
('Global Trade GmbH',        'AT223344556',  'AT');
GO

/*** INVOICES ***/
-- References CUSTOMER IDs 1–10
INSERT INTO INVOICE (INVOICE_DATE, INVOICE_NUM, CUSTOMER_ID, AMOUNT) VALUES
('2026-01-05', 20260001,  1,  1250.0000),
('2026-01-12', 20260002,  2,  3400.5000),
('2026-01-20', 20260003,  3,   875.2500),
('2026-02-03', 20260004,  4,  9999.9900),
('2026-02-14', 20260005,  5,  4200.0000),
('2026-02-28', 20260006,  1,   560.7500),
('2026-03-07', 20260007,  6,  2100.0000),
('2026-03-15', 20260008,  7,  7350.0000),
('2026-03-22', 20260009,  8,  1100.5000),
('2026-04-01', 20260010,  9,  3300.0000),
('2026-04-10', 20260011, 10,   450.0000),
('2026-04-18', 20260012,  2,  6780.2500),
('2026-05-05', 20260013,  3,  1980.0000),
('2026-05-20', 20260014,  5,  8450.0000),
('2026-06-01', 20260015,  4,  2250.7500);
GO

/*** USERS ***/
-- PASSWORD_HASH values are the word 'password' encoded
-- IS_ACTIVE: 1 = active, 0 = inactive
INSERT INTO USERS (USERNAME, PASSWORD_HASH, IS_ACTIVE, REALNAME, LANG) VALUES
('admin',        '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  1, 'Max Mustermann',   'de'),
('j.schmidt',    '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  1, 'Julia Schmidt',    'de'),
('p.dupont',     '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  1, 'Pierre Dupont',    'fr'),
('t.johnson',    '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  1, 'Tom Johnson',      'en'),
('s.rossi',      '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  0, 'Sara Rossi',       'it'),
('viewer01',     '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  1, 'View Only User',   'en'),
('billing01',    '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8',  1, 'Billing Clerk',    'de');
GO

/*** USERS_ROLES ***/
-- Assign roles to users (ROLE IDs: 1=ADMIN, 2=MANAGER, 3=VIEWER, 4=BILLING, 5=SUPPORT)
INSERT INTO USERS_ROLES (USER_ID, ROLE_ID, START_DATE, END_DATE) VALUES
(1, 1, '2025-01-01T00:00:00', NULL),          -- admin → ADMIN (no end date)
(2, 2, '2025-03-01T00:00:00', NULL),          -- j.schmidt → MANAGER
(3, 2, '2025-06-01T00:00:00', NULL),          -- p.dupont → MANAGER
(4, 5, '2025-01-15T00:00:00', NULL),          -- t.johnson → SUPPORT
(5, 3, '2024-01-01T00:00:00', '2026-01-01T00:00:00'), -- s.rossi → VIEWER (expired)
(6, 3, '2025-09-01T00:00:00', NULL),          -- viewer01 → VIEWER
(7, 4, '2025-11-01T00:00:00', NULL),          -- billing01 → BILLING
(2, 5, '2026-01-01T00:00:00', NULL);          -- j.schmidt also has SUPPORT role
GO

/*** USERS_CUSTOMERS ***/
-- Link users to the customers they are responsible for
INSERT INTO USERS_CUSTOMERS (USER_ID, CUSTOMER_ID) VALUES
(2,  1),   -- j.schmidt → Mustermann GmbH
(2,  2),   -- j.schmidt → Schneider AG
(3,  3),   -- p.dupont  → Dupont S.A.S.
(3,  7),   -- p.dupont  → Van den Berg B.V.
(4,  4),   -- t.johnson → Acme Corp.
(4,  5),   -- t.johnson → Tech Solutions Ltd.
(7,  6),   -- billing01 → Rossi S.r.l.
(7,  8),   -- billing01 → Kowalski Sp. z o.o.
(2,  9),   -- j.schmidt → Nordic Supplies AB
(3, 10);   -- p.dupont  → Global Trade GmbH
GO