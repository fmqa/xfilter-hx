CREATE TABLE event(
    id integer primary key,
    type not null,
    connectionStatus
);

CREATE TABLE endpoint(
    id integer primary key,
    uri not null
);

CREATE TABLE egression(
    id integer primary key,
    egressed references event(id),
    egressor references endpoint(id)
);

INSERT INTO event VALUES(1, 'PHONE_CALL','ACCEPTED');
INSERT INTO event VALUES(2, 'PHONE_CALL','REJECTED');
INSERT INTO event VALUES(3, 'PHONE_CALL','ACCEPTED');
INSERT INTO event VALUES(4, 'PHONE_CALL','ACCEPTED');
INSERT INTO event VALUES(5, 'PHONE_CALL','ACCEPTED');
INSERT INTO event VALUES(6, 'PHONE_CALL','ACCEPTED');
INSERT INTO event VALUES(7, 'E_MAIL',NULL);
INSERT INTO event VALUES(8, 'E_MAIL',NULL);
INSERT INTO event VALUES(9, 'E_MAIL',NULL);
INSERT INTO event VALUES(10, 'E_MAIL',NULL);

INSERT INTO endpoint VALUES(1, 'tel:+44-20-8123-4500');
INSERT INTO endpoint VALUES(2, 'tel:+44-20-8123-4501');
INSERT INTO endpoint VALUES(3, 'tel:+44-20-8123-4502');
INSERT INTO endpoint VALUES(4, 'tel:+44-20-8123-4503');
INSERT INTO endpoint VALUES(5, 'tel:+44-20-8123-4504');
INSERT INTO endpoint VALUES(6, 'tel:+44-20-8123-4505');
INSERT INTO endpoint VALUES(7, 'mailto:jim@topmail.com');
INSERT INTO endpoint VALUES(8, 'mailto:bob@topmail.com');
INSERT INTO endpoint VALUES(9, 'mailto:alice@topmail.com');
INSERT INTO endpoint VALUES(10, 'mailto:kate@topmail.com');

INSERT INTO egression(egressed, egressor) VALUES(1, 1);
INSERT INTO egression(egressed, egressor) VALUES(2, 2);
INSERT INTO egression(egressed, egressor) VALUES(3, 3);
INSERT INTO egression(egressed, egressor) VALUES(4, 4);
INSERT INTO egression(egressed, egressor) VALUES(5, 5);
INSERT INTO egression(egressed, egressor) VALUES(6, 6);
INSERT INTO egression(egressed, egressor) VALUES(7, 7);
INSERT INTO egression(egressed, egressor) VALUES(8, 8);
INSERT INTO egression(egressed, egressor) VALUES(9, 9);
INSERT INTO egression(egressed, egressor) VALUES(10, 10);
